#!/usr/bin/perl

use strict;
use warnings;

my ($infile, $outfile, $symsfile) = @ARGV;

open (my $inhandle, "<", $infile)
  or die "$0: can't open $infile for reading: $!";
open (my $outhandle, ">", $outfile)
  or die "$0: can't open $outfile for writing: $!";

my %exposed_vars;
open (my $symshandle, "<", $symsfile)
  or die "$0: can't open $symsfile for reading: $!";
while (<$symshandle>) {
  # $1: var name, $2: type
  $exposed_vars{$1} = $2 if /\s*(.+)\s*:\s*(.+)\s*/;
}
close $symshandle;

while (<$inhandle>) {
  print $outhandle $_;
  while(my ($name, $type) = each %exposed_vars) {
    # remove enclosing parens
    my $line = $_;
    while ($line =~ /\(\s*\b$name\b\s*\)/){
      $line =~ s/\(\s*\b$name\b\s*\)/$name/;
    }
    my $is_printed = 0;
    if ($line =~ /[^\w0-9\.+\-\/=](\&|\-\-|\+\+)*$name(\-\-|\+\+)*[^\w0-9\.+\-\/=]/) {
      my $left = $1;
      my $right = $2;
      if ($left and $left =~ /\&/) {
        print $outhandle "tl_access_$type($name, \"$name\");\n";
        $is_printed = 1;
      }
      if ($left and $left =~ /(\+\+|\-\-)/) {
        print $outhandle "tl_write_$type($name, \"$name\");\n";
        print $outhandle "tl_read_$type($name, \"$name\");\n";
        $is_printed = 1;
      }
      if ($right and $right =~ /(\+\+|\-\-)/) {
        print $outhandle "tl_write_$type($name, \"$name\");\n";
        print $outhandle "tl_read_$type($name, \"$name\");\n";
        $is_printed = 1;
      }
    }
    if ($line =~ /([\w0-9\.+\-\/=]*)[^\w0-9\.+\-\/=]+\b$name\b[^\w0-9\.+\-\/=]+([\w0-9\.+-\/=]*)/) {
      # FIXME: decides just to not log assignment-at-initialization becuase of
      # possibility of static initialization; we should instead log it only if
      # it's within a function
      # is write (assignment, or postinc/dec)
      my $left = $1;
      my $right = $2;
      if ($right =~ /^(=|\+\+|\-\-)/ and not $left =~ /$type/) {
        print $outhandle "tl_write_$type($name, \"$name\");\n";
        if ($right =~ /^(\+\+|\-\-)/) {
          print $outhandle "tl_read_$type($name, \"$name\");\n";
        }
        $is_printed = 1;
      }
      # preinc/dec
      if ($left =~ /(\+\+|\-\-)$/) {
        print $outhandle "tl_write_$type($name, \"$name\");\n";
        print $outhandle "tl_read_$type($name, \"$name\");\n";
        $is_printed = 1;
      }
      # access (address taken)
      if ($left =~ /&/) {
        print $outhandle "tl_access_$type($name, \"$name\");\n";
        $is_printed = 1;
      }
      # read, i guess
      if (not $left =~ /$type/ and not $is_printed) {
        print $outhandle "tl_read_$type($name, \"$name\");\n";
      }
    }
  }
}
close $inhandle;
close $outhandle;
