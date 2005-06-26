#!/usr/bin/pugs

use v6;
use Test;

plan 2;

{
  my $was_in_macro;

  macro postfix:<!> (Int $n) {
    $was_in_macro++;
    my $factorial = [*] 1..$n;
    return "$factorial + 0";
  }

  ok $was_in_macro;
  is 3!, 6, "macro postfix:<!> works", :todo<feature>;
}
