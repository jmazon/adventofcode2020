#! /usr/bin/env perl

use 5.024;
use warnings;

my $valid1 = 0;
my $valid2 = 0;

while (<>) {
  my ($from,$to,$letter,$password) = /^(\d+)-(\d+) (\w): (\w+)$/;
  $valid1++ if $password =~ /^(.*?$letter(*COMMIT)){$from,$to}(?!.*?$letter)/;
  $valid2++ if (substr($password,$from-1,1) eq $letter) xor (substr($password,$to-1,1) eq $letter);
}

say $valid1;
say $valid2;
