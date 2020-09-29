#!/usr/bin/env perl

use strict;
use warnings;

my %exposed_vars;

# use getopt or something to get the optional argument
my ($infile, $outfile, $symsfile) = @ARGV;

if (open(my $insymshandle, "<", $symsfile)){
  while (<$insymshandle>) {
    # $1: var name, $2: type
    $exposed_vars{$1} = $2 if /\s*(.+)\s*:\s*(.+)\s*/;
  }
  close $insymshandle;
}

open(my $inhandle, "<", $infile)
  or die "$0: can't open $infile for reading: $!";
open(my $outhandle, ">", $outfile)
  or die "$0: can't open $outfile for writing: $!";
open(my $outsymshandle, ">>", $symsfile)
  or die "$0: can't open $symsfile for appending: $!";

# just copies the file
while(<$inhandle>){
  # TODO: only parses single full lines, no line splits with semicolon
  # magic. gotta parse those in real life.

  # TODO: somehow get list of system variables that compiler uses so we can tell
  # our user if any of their variables collide with the system vars

  # if we match a TL_EXPOSE_VAR variable declaration
  if (/\s*TL_EXPOSE_VAR\s+([A-Za-z0-9_]+)\s+([A-Za-z0-9_]+)/) {
    # $1: type
    # $2: variable name
    if ($exposed_vars{$2}){
      die "$0: your code has a duplicate identifier among TL_EXPOSE_VARs\n";
    } else {
      $exposed_vars{$2} = $1;   # key = varname, val = type
      print $outsymshandle "$2:$1\n";
    }
  } elsif (/TL_EXPOSE_VAR/) {
    die "$0: you're not initializing watched variables correctly\n";
  }
  # TODO: also check for normal variable declarations and make sure they don't
  # coincide with our TL_EXPOSE_VAR ones (have separate hash of normal
  # declarations, etc)
  # right now, just remove all the expressions we recognizes
  print $outhandle ($_ =~ s/TL_EXPOSE_VAR//r);
}

close $inhandle;
close $outhandle;
close $outsymshandle;
