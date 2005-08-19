#!/usr/bin/pugs

use v6;
use Test;

# http://use.perl.org/~autrijus/journal/25337
# my() declarations scopes lexically to the rest of the block; using $MY::x or
# $::("x") in the block before the actual declaration is erroneous.

plan 6;

{
  dies_ok { my $x; my $x }, "test declare my() variable twice in same scope";

}

{
  dies_ok { $x }, 'my() variable not yet visible (2)';

  my $x = 42;

  is $x, 42, 'my() variable is visible now (2)';
}

{
  my $ret = 42;
  dies_ok { $ret = $x ~ my $x }, 'my() variable not yet visible (1)';
  is $ret, 42,                   'my() variable not yet visible (2)';
}

{
  my $ret = 42;
  lives_ok { $ret = my($x) ~ $x }, 'my() variable is visible (1)', :todo<bug>;
  is $ret, "",                     'my() variable is visible (2)', :todo<bug>;
}
