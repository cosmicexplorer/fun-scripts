# Copyright 2020 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

# Forked from
# https://github.com/pantsbuild/pants/blob/1.25.x-twtr/src/python/pants/backend/python/subsystems/ipex/ipex_launcher.py

"""Entrypoint script for a "dehydrated" .ipex file generated with --generate-ipex.

This script will "hydrate" a normal .pex file in the same directory, then execute it.
"""

# mypy: disallow-untyped-calls, disallow-untyped-defs, disallow-incomplete-defs
# mypy: disallow-untyped-decorators, disallow-any-decorated, disallow-untyped-globals
# mypy: warn-unused-ignores, no-ignore-missing-imports, follow-imports-for-stubs
# mypy: disallow-untyped-globals, follow-imports=normal, disallow-subclassing-any
# mypy: disallow-any-unimported, disallow-any-expr, disallow-any-generics

import json
import os
import shutil
import sys
import tempfile
from typing import Any, cast, Dict, Iterable, List, Optional, Union

from pex import resolver
from pex.common import open_zip
from pex.fetcher import Fetcher, FetcherBase, PyPIFetcher
from pex.interpreter import PythonInterpreter
from pex.pex_builder import PEXBuilder
from pex.pex_info import PexInfo
from pex.variables import Variables


APP_CODE_PREFIX = "user_files/"


def _strip_app_code_prefix(path: str) -> str:
  if not path.startswith(APP_CODE_PREFIX):
    raise ValueError(f"Path {path} in IPEX-INFO did not begin with '{APP_CODE_PREFIX}'.")
  return path[len(APP_CODE_PREFIX):]


def _log(message: str) -> None:
  sys.stderr.write(f"{message}\n")


PexInfoValues = Union[str, Dict[str, str], List[str], bool]


def modify_pex_info(pex_info: PexInfo, **kwargs: PexInfoValues) -> PexInfo:
  new_info: Dict[str, PexInfoValues] = json.loads(pex_info.dump())
  new_info.update(kwargs)
  return PexInfo.from_json(json.dumps(new_info))


class IPEXVariables(Variables):
  """Make use of Pex's existing env var config API, and extend it to ipex-specific features."""

  @property
  def IPEX_SKIP_EXECUTION(self) -> Optional[bool]:
    """Boolean.

    The ipex should *not* execute the hydrated PEX after its dependencies have been hydrated. This
    allows for separating the hydration phase from any later aurora Processes which invoke it.
    Default: false.
    """
    return self._get_bool('IPEX_SKIP_EXECUTION', default=False)

  @property
  def IPEX_USE_UNPACKED_HYDRATED_PEX_DIR(self) -> Optional[bool]:
    """Boolean.

    The ipex should be hydrated into a directory instead of re-zipping into a PEX file. This
    improves the performance of hydration.
    Default: false.
    """
    return self._get_bool('IPEX_USE_UNPACKED_HYDRATED_PEX_DIR', default=False)

  @property
  def IPEX_HYDRATED_PEX_LOCATION(self) -> Optional[str]:
    """String

    If set, write the hydrated PEX to this location. It may be a file or a directory, depending on
    the value of IPEX_USE_UNPACKED_HYDRATED_PEX_DIR. The default behavior is to write the file to a
    filename which incorporates the code_hash of the IPEX for uniqueness.
    """
    return self._get_string('IPEX_HYDRATED_PEX_LOCATION', default=None)

  @property
  def IPEX_USE_CHECKSUMMED_LOCATION(self) -> Optional[bool]:
    """Boolean.

    If true, the default hydrated pex location will incorporate the ipex code_hash. This will avoid
    execing an out-of-date hydrated ipex in cases where the ipex is being updated in-place.

    NB: This does not take effect if IPEX_HYDRATED_PEX_LOCATION is set.
    Default: false.
    """
    # TODO: remove this variable and use the checksummed behavior at all times to avoid silently
    # using old hydration dirs! This would require migrating all consumers to use
    # IPEX_HYDRATED_PEX_LOCATION if they require a specific path.
    return self._get_bool('IPEX_USE_CHECKSUMMED_LOCATION', default=False)

  @property
  def IPEX_HYDRATION_PLATFORM(self) -> Optional[str]:
    """String

    If set, use this platform for hydrating dependencies. Valid platform strings include:
    - macosx_10_12_x86_64
    - linux_x86_64

    If not set, defaults to the 'current' platform string.

    TODO: Note that the valid platform strings will change after switching ipex to use pex 2!
    """
    return self._get_string('IPEX_HYDRATION_PLATFORM', default=None)

  @classmethod
  def _special_ipex_config_var_names(cls) -> Iterable[str]:
    """Get the configuration variables specific to ipex.

    These variables do not affect the execution of the hydrated PEX file.
    """
    return [var for var in dir(cls) if var.startswith('IPEX_')]

  def patch_ipex_vars(self) -> Dict[str, str]:
    """Convert IPEX_* vars into PEX_* vars, except for special ipex config vars.

    This is necessary because some PEX_* variables (specifically PEX_PATH) are erased from the
    environment before executing this script. Providing the same variables, with the IPEX_* prefix,
    allows safely propagating them to the execution of the hydrated PEX.

    Importantly, this means that command lines can easily be created which will work the *exact*
    same with both an IPEX *or* a normal PEX, by providing all the PEX_* variables with the prefix
    changed to IPEX_* as well.

    For example, these two command lines would work the exact same:

    > IPEX_PATH=other-pex.pex PEX_PATH=other.pex.pex ./myfile.ipex
    > IPEX_PATH=other-pex.pex PEX_PATH=other.pex.pex ./myfile.pex

    If PEX_VERBOSE is provided, that will provide highly detailed debugging information during
    hydration. Other variables such as PEX_PROFILE are also respected.
    """
    special_ipex_vars = frozenset(self._special_ipex_config_var_names())
    result_env = self._environ.copy()
    for k, v in self._environ.items():
      if k.startswith('IPEX_'):
        if k in special_ipex_vars:
          # Do not copy over the special ipex config variables to the hydrated PEX execution env.
          del result_env[k]
        else:
          # Strip the initial 'I' from the var name.
          pex_var = k[len('I'):]
          result_env[pex_var] = v
    return result_env


# Analog to pex.variables.ENV.
# This constructor reads from os.environ and all pexrc locations (/etc/pexrc, ~/.pexrc).
IPEX_ENV = IPEXVariables()


IPEX_RESOLVER_SETTINGS_JSON = Dict[str, Union[List[str], bool]]


class IpexResolverSettings:
  def __init__(
    self,
    indexes: List[str],
    find_links: List[str],
    allow_prereleases: bool,
    use_manylinux: bool,
  ) -> None:
    self.indexes = indexes
    self.find_links = find_links
    self.allow_prereleases = allow_prereleases
    self.use_manylinux = use_manylinux

  @classmethod
  def from_json(cls, content: str) -> "IpexResolverSettings":
    data = cast(IPEX_RESOLVER_SETTINGS_JSON, json.loads(content))
    return cls(
      indexes=cast(List[str], data["indexes"]),
      find_links=cast(List[str], data["find_links"]),
      allow_prereleases=cast(bool, data["allow_prereleases"]),
      use_manylinux=cast(bool, data["use_manylinux"]),
    )


IPEX_INFO_JSON = Dict[str, Union[List[str], IPEX_RESOLVER_SETTINGS_JSON]]


class IpexInfo:

  def __init__(self, code: List[str], resolver_settings: IpexResolverSettings) -> None:
    self.code = code
    self.resolver_settings = resolver_settings

  @classmethod
  def from_json(cls, content: str) -> "IpexInfo":
    data = cast(IPEX_INFO_JSON, json.loads(content))
    return cls(
      code=cast(List[str], data["code"]),
      resolver_settings=IpexResolverSettings.from_json(json.dumps(data["resolver_settings"])),
    )


def _hydrate_pex_file(self: str, hydrated_pex_location: str) -> None:
  # We extract source files into a temporary directory before creating the pex.
  td = tempfile.mkdtemp()

  with open_zip(self) as zf:
    # Populate the pex with the pinned requirements and distribution names & hashes.
    bootstrap_info = PexInfo.from_json(zf.read("BOOTSTRAP-PEX-INFO"))
    bootstrap_builder = PEXBuilder(pex_info=bootstrap_info, interpreter=PythonInterpreter.get())

    # Populate the pex with the needed code.
    try:
      ipex_info = IpexInfo.from_json(zf.read("IPEX-INFO").decode("utf-8"))
      for path in ipex_info.code:
        unzipped_source = zf.extract(path, td)
        bootstrap_builder.add_source(
          unzipped_source, env_filename=_strip_app_code_prefix(path)
        )
    except Exception as e:
      raise ValueError(f"Error: {e}. The IPEX-INFO for this .ipex file was:\n{ipex_info}")

  # Perform a fully pinned *in*transitive resolve to hydrate the install cache.
  # TODO: After upgrading --pex-builder-wrapper-pex-version to use the latest pex 2 release for
  # performance, the arguments `find_links` and `indexes` can be provided directly to the
  # `resolve()` call.
  fetchers: List[FetcherBase] = [
    *[Fetcher([url]) for url in ipex_info.resolver_settings.find_links],
    *[PyPIFetcher(url) for url in ipex_info.resolver_settings.indexes],
  ]

  resolved_distributions = resolver.resolve(
    requirements=bootstrap_info.requirements,
    cache=bootstrap_info.pex_root,
    platform=(IPEX_ENV.IPEX_HYDRATION_PLATFORM or "current"),
    interpreter=bootstrap_builder.interpreter,
    fetchers=fetchers,
    allow_prereleases=ipex_info.resolver_settings.allow_prereleases,
    use_manylinux=ipex_info.resolver_settings.use_manylinux,
    transitive=False,
  )
  # TODO: It's not yet clear why this is necessary, as we should be able to use the same
  # 'distributions' from BOOTSTRAP-PEX-INFO. When the .ipex is executed, the normal pex bootstrap
  # fails to see these requirements or recognize that they should be pulled from the cache for some
  # reason. Removing the need for the separate .add_distribution() here will improve hydration
  # performance. This may be fixed in pex 2 (and if not, upstream pex would love to help fix it).
  for resolved_dist in resolved_distributions:
    bootstrap_builder.add_distribution(resolved_dist.distribution)

  # NB: Bytecode compilation can take an extremely long time for large 3rdparty modules.
  shared_builder_kwargs = dict(bytecode_compile=False)
  if IPEX_ENV.IPEX_USE_UNPACKED_HYDRATED_PEX_DIR:
    # We call .freeze() instead of .build() here in order to avoid zipping up all the large 3rdparty
    # modules we just added to the chroot. This improves performance as the python stdlib zip
    # implementation is known to be particularly slow.
    bootstrap_builder.freeze(**shared_builder_kwargs)
    # Because the PEXBuilder will use a temporary directory by default, and /tmp may be on a
    # separate partition on some machines, we copy over the directory to the desired location.
    # TODO: Hydrate the full PEX file in-place in `hydrated_pex_location` in the first place.
    shutil.copytree(bootstrap_builder.path(), hydrated_pex_location, symlinks=True)
  else:
    # TODO: Remove this branch, and unconditionally create an *unpacked* PEX dir, but then also
    # generate a short runner script right next to it which will execute the unpacked hydrated PEX
    # with the appropriate shebang line.  This would give us the ease of use of the PEX file,
    # without zipping anything up.
    bootstrap_builder.build(hydrated_pex_location, **shared_builder_kwargs)


def _determine_hydrated_pex_location(self: str) -> str:
  filename_base, _ext = os.path.splitext(self)

  hydrated_pex_location: str
  if IPEX_ENV.IPEX_HYDRATED_PEX_LOCATION:
    hydrated_pex_location = IPEX_ENV.IPEX_HYDRATED_PEX_LOCATION
  elif IPEX_ENV.IPEX_USE_CHECKSUMMED_LOCATION:
    # Incorporate the code hash into the output unpacked pex directory in order to:
    # (a) avoid execing an out of date hydrated ipex,
    # (b) avoid collisions with other similarly-named (i)pex files in the same directory!
    code_hash = PexInfo.from_pex(self).code_hash
    # Since we're producing a "real" PEX file now (with all of its necessary dependencies), and we
    # use the code hash to differentiate it from the source IPEX, we can just name it with a .pex
    # suffix.
    hydrated_pex_location = f"{filename_base}-{code_hash}.pex"
  else:
    # TODO: remove this variable and use the checksummed behavior at all times to avoid silently
    # using old hydration dirs! This would require migrating all consumers to use
    # IPEX_HYDRATED_PEX_LOCATION if they require a specific path.
    hydrated_pex_location = f"{filename_base}.pex"
    _log(f'IPEX_USE_CHECKSUMMED_LOCATION defaults to false, so creating an output pex at {hydrated_pex_location}. '
         'This variable will soon go away to ensure that the ipex hydration dir is invalidated '
         'whenever the ipex is modified. Please set IPEX_HYDRATED_PEX_LOCATION and '
         'IPEX_USE_UNPACKED_HYDRATED_PEX_DIR if your use case currently involves unzipping the '
         'resulting PEX file after hydration. DPB-12898 will document these variables better.')

  return hydrated_pex_location


def main(self: str) -> None:
  hydrated_pex_location = _determine_hydrated_pex_location(self)

  # NB: This location currently may be a PEX file *or* a directory.
  if not os.path.exists(hydrated_pex_location):
    _log(f"Hydrating {self} to {hydrated_pex_location}...")
    _hydrate_pex_file(self, hydrated_pex_location)

  if IPEX_ENV.IPEX_SKIP_EXECUTION:
    _log("*Avoiding* execution of pex file and exiting cleanly!")
  else:
    inner_argv = [sys.executable, hydrated_pex_location] + sys.argv[1:]
    patched_pex_vars = IPEX_ENV.patch_ipex_vars()
    _log(f"Executing hydrated pex file, patching IPEX_* vars into PEX_* vars: {patched_pex_vars}")
    os.execve(sys.executable, inner_argv, patched_pex_vars)


if __name__ == "__main__":
  self = sys.argv[0]
  main(self)
  sys.exit(0)
