#!/usr/bin/pugs

use v6;
use Test;

plan 8;

{
  my @a;
  is @a.elems, 0, ".elems works on uninitialized arrays";
}

{
  my @a = ();
  is @a.elems, 0, ".elems works on empty arrays";
}

{
  my @a = <a b c>;
  is @a.elems, 3, ".elems works on initialized arrays";
}

{
  my $a;
  dies_ok { $a.elems }, ".elems does not work on arbitrary scalars (1)";
}

{
  my $a = 42;
  dies_ok { $a.elems }, ".elems does not work on arbitrary scalars (2)";
}

{
  my $a = [];
  is $a.elems, 0, ".elems works on empty arrayrefs";
}

{
  my $a = [<a b c>];
  is $a.elems, 3, ".elems works on initialized arrayrefs (1)";
}

{
  my $a = <a b c>;
  is $a.elems, 3, ".elems works on initialized arrayrefs (2)";
}
