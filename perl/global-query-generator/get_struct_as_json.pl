#!/usr/bin/perl

use strict;
use warnings;

use Data::Dumper;

my ($infile) = @ARGV;

my %structs_found;
my %unions_found;
my %typedef_table;
my %enums_found;
my %vars_found;

my $handling_struct = undef;
my $in_anon_struct = undef;
my $current_indentation = 0;

my $marker = "---";

# TODO: close $inhandle!

open(my $inhandle, "<", $infile)
  or die "$0: can't open $infile for reading: $!";
while (<$inhandle>) {
  $current_indentation = get_indentation($_);
  handle_current_line($current_indentation, $_, $inhandle,
                      \%structs_found,
                      \%unions_found,
                      \%typedef_table,
                      \%enums_found,
                      \%vars_found);
}

print $marker . "structs:$marker\n";
while (my ($k, $v) = each %structs_found) {
  print "$k:$v\n";
}

print $marker . "unions:$marker\n";
while (my ($k, $v) = each %unions_found) {
  print "$k:$v\n";
}

print $marker . "typedefs:$marker\n";
while (my ($k, $v) = each %typedef_table) {
  print "$k:$v\n";
}

print $marker . "enums:$marker\n";
while (my ($k, $v) = each %enums_found) {
  print "$k:$v\n";
}

print $marker . "vars:$marker\n";
while (my ($k, $v) = each %vars_found) {
  print "$k:$v\n";
}

# END MAIN
# BEGIN SUBS

sub handle_current_line {
  my ($indent, $line, $fh,
      $structs_found_ref,
      $unions_found_ref,
      $typedef_table_ref,
      $enums_found_ref,
      $vars_found_ref) = @_;

  # handle function decls
  if ($line =~ /FunctionDecl/) {
    handle_function_decl($indent, $fh);
  }

  # handle enums
  if ($line =~ /EnumDecl/) {
    if ($line =~ /> col:[0-9]+ ([_a-zA-Z0-9]+)/) {
      # top-level named enum defn
      $enums_found_ref->{$1} =
        get_keys_as_string(get_enum_fields($indent, $fh));
    } else {
      handle_anon_enum_decl($indent, $line, $fh, {}, $vars_found_ref);
    }
  }

  # note: recorddecls within recorddecls are always anonymous structs (aka
  # structs followed immediately by typedefs)

  # handle unions
  if ($line =~ /union definition/) {
    # top-level anon union defn
    handle_anon_union_decl($indent, $fh, {},
                           $vars_found_ref, $unions_found_ref);
  } elsif ($line =~ /union (.*) definition/) {
    # top-level named union defn
    $unions_found_ref->{$1} =
      get_hash_as_string(get_struct_fields($indent, $fh));
  }

  # handle structs
  if ($line =~ /struct definition/) {
    # top-level anon struct defn
    handle_anon_struct_decl($indent, $fh, {},
                            $vars_found_ref, $structs_found_ref);
  } elsif ($line =~ /struct (.*) definition/) {
    # top-level named struct defn
    $structs_found_ref->{$1} =
      get_hash_as_string(get_struct_fields($indent, $fh));
  }

  # handle typedefs
  # note: typedefdecls only occur at top level
  if ($line =~ /TypedefDecl/) {
    $typedef_table_ref->{$1} = $2
      if ($line =~ /TypedefDecl.* ([_a-zA-Z0-9]+) \'([_a-zA-Z0-9]+)\'/);
    return;
  }
}

sub get_indentation {
  my ($line) = @_;
  return length($1) / 2 if ($line =~ (/^([\| `]*)[\|`]-[^\|`]/));
}

sub advance_past_cur_field {
  my ($target_indentation, $fh) = @_;
  while (<$fh>) {
    $current_indentation = get_indentation($_);
    if (($current_indentation + 1) eq $target_indentation and
        /`\-/) {
      return;
    }
  }
}

sub handle_function_decl {
  my ($indent, $fh) = @_;
  advance_past_cur_field($indent, $fh);
}

sub handle_anon_union_decl {
  my ($indent, $fh, $cur_struct_ref,
      $vars_found_ref, $unions_found_ref) = @_;
  my $struct_ref = get_struct_fields($indent, $fh);
  my $line = <$fh>;
  if (not $line =~ /(TypedefDecl|VarDecl|FieldDecl)/) {
    die "$0: ERROR anon union declared without corresponding variable\n";
  } else {
    my $union_desc = "(anon_union) " . get_hash_as_string($struct_ref);
    if ($1 eq "VarDecl") {
      $vars_found_ref->{$1} = $union_desc
        if ($line =~ /> col:[0-9]+ ([_a-zA-Z0-9]+)/);
    } elsif ($1 eq "FieldDecl") {
      $cur_struct_ref->{$1} = $union_desc
        if ($line =~ /> col:[0-9]+ ([_a-zA-Z0-9]+)/);
    } elsif ($1 eq "TypedefDecl") {
      if (not defined $unions_found_ref) {
        die "$0: ERROR FAILURE TO LAUNCH SHOULD NEVER HAPPEN2\n";
      }
      $unions_found_ref->{$1} = $union_desc
        if ($line =~ /([_a-zA-Z0-9]+) \'([_a-zA-Z0-9]+)\' *$/);
    } else {
      die "$0: ERROR should not be here\n";
    }
  }
}

sub handle_anon_struct_decl {
  my ($indent, $fh,
      $cur_struct_ref, $vars_found_ref, $structs_found_ref) = @_;
  my $struct_ref = get_struct_fields($indent, $fh);
  my $line = <$fh>;
  if (not $line =~ /(TypedefDecl|VarDecl|FieldDecl)/) {
    print Dumper($struct_ref);
    print $line;
    die "$0: ERROR anon struct declared without corresponding variable\n";
  } else {
    my $struct_desc = get_hash_as_string($struct_ref);
    if ($1 eq "VarDecl") {
      $vars_found_ref->{$1} = $struct_desc
        if ($line =~ /> col[0-9]+ ([_a-zA-Z0-9]+)/);
    } elsif ($1 eq "FieldDecl") {
      $cur_struct_ref->{$1} = $struct_desc
        if ($line =~ /> col[0-9]+ ([_a-zA-Z0-9]+)/);
    } elsif ($1 eq "TypedefDecl") {
      if (not defined $structs_found_ref) {
        die "$0: ERROR FAILURE TO LAUNCH SHOULD NEVER HAPPEN\n";
      }
      $structs_found_ref->{$1} = $struct_desc
        if ($line =~ /([_a-zA-Z0-9]+) \'([_a-zA-Z0-9]+)\' *$/);
    } else {
      die "$0: ERROR should not be here\n";
    }
  }
}

sub handle_anon_enum_decl {
  my ($indent, $fh, $cur_struct_ref, $vars_found_ref) = @_;
  my @enum_list = get_enum_fields($indent, $fh);
  my $line = <$fh>;
  if (not $line =~ /(VarDecl|FieldDecl)/) {
    die "$0: ERROR anon enum declared without corresponding variable\n";
  } else {
    my $enum_desc = get_list_as_string(@enum_list);
    if ($1 eq "VarDecl") {
      $vars_found_ref->{$1} = $enum_desc
        if ($line =~ /> col[0-9]+ ([_a-zA-Z0-9]+)/);
    } elsif ($1 eq "FieldDecl") {
      $cur_struct_ref->{$1} = $enum_desc
        if ($line =~ /> col[0-9]+ ([_a-zA-Z0-9]+)/);
    } else {
      die "$0: ERROR should not be here\n";
    }
  }
}

sub get_enum_fields {
  my ($indent, $fh) = @_;
  my @list;
  while (<$fh>) {
    if (/EnumConstantDecl.* ([_a-zA-Z0-9]+) \'int\' *$/) {
      unshift(@list, $1);
    }
    if ($indent eq (get_indentation($_) - 1) and
        /`-/) {
      return @list;
    }
  }
  return @list;
}

sub get_struct_fields {
  my ($indent, $fh) = @_;
  my $fields_hash_ref = {};
  while (<$fh>) {
    if (/FieldDecl.* ([_a-zA-Z0-9]+) \'(.*)\' *$/) {
      $fields_hash_ref->{$1} = $2;
    } elsif (/RecordDecl.*(struct|union) definition/) {
      if ($1 eq "struct") {
        handle_anon_struct_decl($indent + 1, $fh,
                                $fields_hash_ref);
      } elsif ($1 eq "union") {
        handle_anon_union_decl($indent + 1, $fh,
                               $fields_hash_ref);
      } else {
        die "$0: ERROR should not be here\n";
      }
    }
    if ($indent eq (get_indentation($_) - 1) and
        /`-/) {
      return $fields_hash_ref;
    }
  }
  die "ALSO NOT HERE LOL\n";
}

sub get_hash_as_string {
  my ($hash_ref) = @_;
  my $str = "{";
  while (my ($k, $v) = each %{$hash_ref}) {
    $str = $str . "$k:$v,";
  }
  $str = $str . "}";
  return $str;
}

sub get_list_as_string {
  my (@list) = @_;
  my $str = "{";
  foreach my $entry (@list) {
    $str = $str . "$entry,";
  }
  $str = $str . "}";
  return $str;
}
