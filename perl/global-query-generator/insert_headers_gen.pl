#!/usr/bin/env perl

use strict;
use warnings;

my $ifdef_header_regex = "INSERT_HEADER_FOR_GEN";

my ($outfile) = @ARGV;
my $infile = ($outfile =~ s/\.c/\.ttmp/r);

my $do_replace = 0;
my $is_replacing = 0;

open(my $inhandle, "<", $infile)
  or die "$0: can't open $infile for reading: $!";
open(my $outhandle, ">", $outfile)
  or die "$): can't open $outfile for writing: $!";
while (<$inhandle>) {
  if ($_ =~ /$ifdef_header_regex/ and not $do_replace){
    $is_replacing = 1;
    $do_replace = 1;
    insert_headers($outhandle);
  }
  if ($_ =~ /$ifdef_header_regex/ and $do_replace and not $is_replacing){
    $is_replacing = 1;
    $do_replace = 0;
  }
  if (not $do_replace and not $is_replacing) {
    print $outhandle $_;
  }
  $is_replacing = 0;
}
close $inhandle;
close $outhandle;

sub insert_headers {
  my ($out_handle) = @_;
  my @ls_out = `ls *.h`;
  foreach my $header (@ls_out) {
    $header =~ s/\n$//g;
    print $out_handle "#include \"$header\"\n";
  }
}
