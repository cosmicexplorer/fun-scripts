#!/usr/bin/env perl

use strict;
use warnings;

my %exposed_vars;

my ($infile, $outfile) = @ARGV;

open (my $inhandle, "<", $infile)
  or die "$0: can't open $infile for reading: $!";
while (<$inhandle>) {
  # $1: var name, $2: type
  $exposed_vars{$1} = $2 if /\s*(.+)\s*:\s*(.+)\s*/;
}
close $inhandle;

open (my $outhandle, ">", $outfile)
  or die "$0: can't open $outfile for writing: $!";

print $outhandle "#ifndef TLCLASSES_H\n";
print $outhandle "#define TLCLASSES_H\n";
print $outhandle "#include <sys/syscall.h>\n";
print $outhandle "#include <sys/types.h>\n";
print $outhandle "#include <unistd.h>\n\n";

my %unique_types;
foreach my $type (values %exposed_vars) {
  $unique_types{$type} = 1;
}
foreach my $uniq_type (keys %unique_types) {
  # can only do types which can be printf'd with %d
  # TODO: would prefer to pass in printf specifier when exposing variable, or
  # pointer to function which will output the value of the variable in a char *
  my $format_spec = "%d";
  if ($uniq_type eq "char") {
    $format_spec = "%c";
  } elsif ($uniq_type eq "double") {
    $format_spec = "%f";
  }
  # read function (= on left, or in arithmetic expr)
  print $outhandle "$uniq_type tl_read_$uniq_type($uniq_type t, char *str)\n";
  print $outhandle "{\n";
  print $outhandle "\tpid_t pid = getpid();\n";
  print $outhandle "\tpid_t tid = syscall(SYS_gettid);\n";
  print $outhandle "\tfprintf(stderr,\"read:val=$format_spec,pid=%d,tid=%d,%s\\n\", t, pid, tid, str);\n";
  print $outhandle "\treturn t;\n";
  print $outhandle "}\n";
  # write function (= on right, ++/--)
  print $outhandle "$uniq_type tl_write_$uniq_type($uniq_type t, char *str)\n";
  print $outhandle "{\n";
  print $outhandle "\tpid_t pid = getpid();\n";
  print $outhandle "\tpid_t tid = syscall(SYS_gettid);\n";
  print $outhandle "\tfprintf(stderr,\"write:val=$format_spec,pid=%d,tid=%d,%s\\n\", t, pid, tid, str);\n";
  print $outhandle "\treturn t;\n";
  print $outhandle "}\n";
  # access function (address taken)
  print $outhandle "$uniq_type tl_access_$uniq_type($uniq_type t, char *str)\n";
  print $outhandle "{\n";
  print $outhandle "\tpid_t pid = getpid();\n";
  print $outhandle "\tpid_t tid = syscall(SYS_gettid);\n";
  print $outhandle "\tfprintf(stderr,\"access:val=$format_spec,pid=%d,tid=%d,%s\\n\", t, pid, tid, str);\n";
  print $outhandle "\treturn t;\n";
  print $outhandle "}\n";
  print $outhandle "\n";
}

print $outhandle "#endif /* TLCLASSES_H */\n";

close $outhandle;
