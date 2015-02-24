#!/usr/bin/env perl

open(my $devrand, "<", "/dev/urandom") or die ("lol");

read($devrand, my $bytes, 16);

print "$bytes\n";
