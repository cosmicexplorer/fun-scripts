#!/usr/bin/perl

use strict;
use warnings;

my $ifdef_regex = "INSERT_FOR_GEN";

my ($vars_file, $structs_file, $template_file, $out_file) = @ARGV;

my $is_header = 0;

if ($out_file =~ /\.h$/){
  $is_header = 1;
}

if ($out_file =~ /\.c$/) {
  $out_file =~ s/\.c/\.ttmp/g;
}

my $do_replace = 0;
my $is_replacing = 0;

open(my $tmpl_handle, "<", $template_file)
  or die "$0: can't open $template_file for reading: $!";
open(my $out_handle, ">", $out_file)
  or die "$0: can't open $out_file for writing: $!";
while (<$tmpl_handle>){
  if ($_ =~ /$ifdef_regex/ and not $do_replace){
    $is_replacing = 1;
    $do_replace = 1;
    if ($is_header){
      format_header($vars_file, $structs_file, $out_handle);
    } else {
      format_source($vars_file, $structs_file, $out_handle);
    }
  }
  if ($_ =~ /$ifdef_regex/ and $do_replace and not $is_replacing){
    $is_replacing = 1;
    $do_replace = 0;
  }
  if (not $do_replace and not $is_replacing) {
    print $out_handle $_;
  }
  $is_replacing = 0;
}
close $tmpl_handle;
close $out_handle;

sub format_header {
  my ($varsfile, $structsfile, $outfiledesc) = @_;
  print $outfiledesc "void ___gen_print_selection(size_t);\n\n";
  my %vars_hash = read_yaml_to_hash($varsfile);
  my $index = 0;
  while (my ($k, $v) = each %vars_hash) {
    $index = $index + 1;
  }
  print $outfiledesc "\n#define ___TOTAL_GEN $index\n\n";

  my %structs_hash = read_yaml_to_hash($structsfile);
  while (my ($k, $v) = each %structs_hash) {
    print $outfiledesc "void ___gen_print_$k(struct $k s);\n\n";
  }

}

sub format_source {
  my ($varsfile, $structsfile, $outfiledesc) = @_;
  my %vars_hash = read_yaml_to_hash($varsfile);
  print $outfiledesc "void ___gen_print_selection(size_t s) {\n";
  print $outfiledesc "\tswitch (s) {\n";
  my $index = 0;
  my $format_spec = "";
  while (my ($k, $v) = each %vars_hash) {
    print $outfiledesc "\tcase $index:\n";
    if ($v =~ /\*/){
      print $outfiledesc "\t\tif (NULL == $k) {\n";
      print $outfiledesc "\t\t\tprintf(\"[null]\\n\");\n";
      print $outfiledesc "\t\t} else {\n";
      print $outfiledesc "\t";
    }
    if ($v eq "int") {
      print $outfiledesc "\t\tprintf(\"%d\\n\", $k);\n";
    } elsif ($v eq "_Bool") {
      print $outfiledesc "\t\tprintf(\"%d\\n\", $k);\n";
    } elsif ($v eq "signed char") {
      print $outfiledesc "\t\tprintf(\"%hhd\\n\", $k);\n";
    } elsif ($v eq "short int") {
      print $outfiledesc "\t\tprintf(\"%hd\\n\", $k);\n";
    } elsif ($v eq "short") {
      print $outfiledesc "\t\tprintf(\"%hd\\n\", $k);\n";
    } elsif ($v eq "long int") {
      print $outfiledesc "\t\tprintf(\"%ld\\n\", $k);\n";
    } elsif ($v eq "long") {
      print $outfiledesc "\t\tprintf(\"%ld\\n\", $k);\n";
    } elsif ($v eq "short") {
      print $outfiledesc "\t\tprintf(\"%hd\\n\", $k);\n";
    } elsif ($v eq "long long int") {
      print $outfiledesc "\t\tprintf(\"%lld\\n\", $k);\n";
    } elsif ($v eq "long long") {
      print $outfiledesc "\t\tprintf(\"%lld\\n\", $k);\n";
    } elsif ($v eq "intmax_t") {
      print $outfiledesc "\t\tprintf(\"%kd\\n\", $k);\n";
    } elsif ($v eq "size_t") {
      print $outfiledesc "\t\tprintf(\"%zu\\n\", $k);\n";
    } elsif ($v eq "ptrdiff_t") {
      print $outfiledesc "\t\tprintf(\"%td\\n\", $k);\n";
    } elsif ($v eq "unsigned int") {
      print $outfiledesc "\t\tprintf(\"%u\\n\", $k);\n";
    } elsif ($v eq "unsigned") {
      print $outfiledesc "\t\tprintf(\"%u\\n\", $k);\n";
    } elsif ($v eq "unsigned char") {
      print $outfiledesc "\t\tprintf(\"%hhu\\n\", $k);\n";
    } elsif ($v =~ "unsigned short") {
      print $outfiledesc "\t\tprintf(\"%hu\\n\", $k);\n";
    } elsif ($v =~ "unsigned long") {
      print $outfiledesc "\t\tprintf(\"%lu\\n\", $k);\n";
    } elsif ($v =~ "unsigned long long") {
      print $outfiledesc "\t\tprintf(\"%llu\\n\", $k);\n";
    } elsif ($v eq "uintmax_t") {
      print $outfiledesc "\t\tprintf(\"%ju\\n\", $k);\n";
    } elsif ($v eq "double") {
      print $outfiledesc "\t\tprintf(\"%f\\n\", $k);\n";
    } elsif ($v eq "long double") {
      print $outfiledesc "\t\tprintf(\"%Lf\\n\", $k);\n";
    } elsif ($v eq "char *") {
      print $outfiledesc "\t\tprintf(\"%s\\n\", $k);\n";
    } elsif ($v eq "wchar_t *") {
      print $outfiledesc "\t\tprintf(\"%ls\\n\", $k);\n";
    } elsif ($v eq "void *") {
      print $outfiledesc "\t\tprintf(\"%p\\n\", $k);\n";
    } elsif ($v eq "int *") {
      print $outfiledesc "\t\tprintf(\"%n\\n\", $k);\n";
    } elsif ($v eq "signed char *") {
      print $outfiledesc "\t\tprintf(\"%hhn\\n\", $k);\n";
    } elsif ($v eq "short int *") {
      print $outfiledesc "\t\tprintf(\"%hn\\n\", $k);\n";
    } elsif ($v eq "short *") {
      print $outfiledesc "\t\tprintf(\"%hn\\n\", $k);\n";
    } elsif ($v eq "long int *") {
      print $outfiledesc "\t\tprintf(\"%ln\\n\", $k);\n";
    } elsif ($v eq "long *") {
      print $outfiledesc "\t\tprintf(\"%ln\\n\", $k);\n";
    } elsif ($v eq "long long int *") {
      print $outfiledesc "\t\tprintf(\"%lln\\n\", $k);\n";
    } elsif ($v eq "long long *") {
      print $outfiledesc "\t\tprintf(\"%lln\\n\", $k);\n";
    } elsif ($v eq "intmax_t *") {
      print $outfiledesc "\t\tprintf(\"%jn\\n\", $k);\n";
    } elsif ($v eq "size_t *") {
      print $outfiledesc "\t\tprintf(\"%zn\\n\", $k);\n";
    } elsif ($v eq "ptrdiff_t *") {
      print $outfiledesc "\t\tprintf(\"%tn\\n\", $k);\n";
    } else {
      if ($v =~ /struct/) {
        my $tmp = ($v =~ s/struct //gr);
        if ($v =~ /\*/){         # pointer to struct
          $tmp =~ s/ *//g;
          print $outfiledesc "\t\t___gen_print_$tmp(*$k);\n";
        } else {
          print $outfiledesc "\t\t___gen_print_$tmp($k);\n";
        }
      } else {
        print $outfiledesc "\t\tprintf(\"%d\\n\", $k);\n";
      }
    }
    if ($v =~ /\*/){
      print $outfiledesc "\t\t}\n";
    }
    print $outfiledesc "\t\tbreak;\n";
    $index = $index + 1;
  }
  print $outfiledesc "\tdefault:\n";
  print $outfiledesc "\t\tprintf(\"unspecified!\\n\");\n";
  print $outfiledesc "\t}\n";
  print $outfiledesc "}\n\n";

  my %structs_hash = read_yaml_to_hash($structsfile);
  while (my ($k, $v) = each %structs_hash) {
    print $outfiledesc "void ___gen_print_$k(struct $k s) {\n";
    print $outfiledesc
      "\tfprintf(stderr, \"not yet implemented (struct $k)!\\n\");\n";
    print $outfiledesc "}\n";
  }
}

sub read_yaml_to_hash {
  my ($infile) = @_;
  my @lines = `cat $infile | grep ":"`;
  my %yaml;
  my ($key, $val) = (undef, undef);
  foreach my $line (@lines){
    $key = ((($line =~ s/:.*$//gr) =~ s/^[[:space:]]+//r) =~ s/\n//r);
    $val = (($line =~ s/.*: //gr) =~ s/\n//r);
    $yaml{$key} = $val;
  }
  return %yaml;
}
