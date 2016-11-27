#!/bin/perl

while (<STDIN>) {
  if (/^([^,]+),PT(?:([0-9]+)H)?(?:([0-9]+)M)?(?:([0-9]+)S)?$/m) {
    print "$1,";
    print $2 * 360 + $3 * 60 + $4;
  }
  print("\n");
}
