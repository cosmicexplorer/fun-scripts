import re
from types import ModuleType
from typing import Any, Iterator, Match, Pattern, Tuple, Union


def traverse_module(mod: ModuleType) -> Iterator[Tuple[str, Any]]:
    for name in dir(mod):
        member = getattr(mod, name)
        yield name, member


def glob_module(mod: ModuleType, regex: Union[Pattern, str]) -> Iterator[Tuple[str, Any, Match]]:
    if isinstance(regex, str):
        regex = re.compile(regex)
    for name, member in traverse_module(mod):
        match = re.search(regex, name)
        print (regex, name, match, mod)
        if match:
            yield name, member, match


def apropos_module(mod: ModuleType, regex: Union[Pattern, str]) -> None:
    print('\n\n'.join(
        '{}:\n{}'.format(name, getattr(member, '__doc__', '<no docstring>'))
        for name, member, match in glob_module(mod, regex)
    ))

ap = apropos_module
