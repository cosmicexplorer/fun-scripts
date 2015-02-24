#!/usr/bin/env perl

use strict;
use warnings;

my $guesses_made = 0;

print "Hello; what is your name!\n";

my $name = <>;
chomp($name);

my $cur_num = int(rand(20));

print "$name, I'm thinking of a number between 1 and 20!!!!!\n";

my $guess;

while ($guesses_made < 6){
  print "Take a guess: ";
  ++$guesses_made;
  $guess = int(<STDIN>);
  if ($guess < $cur_num){
    print "Your guess is too low!!!!\n";
  } elsif ($guess > $cur_num){
    print "Your guess is too high!!!\n";
  } else {                      # $guess == $cur_num
    last;
  }
}

if ($guess == $cur_num){
  print "YOU DID IT, $name!!!!\n";
} else {
  print "lol $name you nerd\n";
}
