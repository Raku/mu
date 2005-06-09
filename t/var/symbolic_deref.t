#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# L<S02/"Names and Variables" /All symbolic references are done with this notation:/>
{
  my $a_var = 42;
  my $b_var = "a_var";

  is $::($b_var), 42, 'basic symbolic scalar dereferentiation works';
}

{
  my @a_var = <a b c>;
  my $b_var = "a_var";

  is @::($b_var)[1], "b", 'basic symbolic array dereferentiation works';
}

{
  my %a_var = (a => 42);
  my $b_var = "a_var";

  is %::($b_var)<a>, 42, 'basic symbolic hash dereferentiation works';
}

{
  my &a_var = { 42 };
  my $b_var = "a_var";

  is &::($b_var)(), 42, 'basic symbolic code dereferentiation works';
}

{
  my $pugs::is::cool = 42;
  my $cool = "cool";

  is $::("pugs")::is::($cool), 42, 'not so basic symbolic dereferentiation works';
}

{
  my $a_var = 42;
  my $sub   = sub { $::("CALLER")::("a_var") };
  is $sub(), 42, "symbolic dereferentation works with ::CALLER, too";
}

# Symbolic dereferentiation of type vars
{
  cmp_ok ::Array, &infix:<=:=>, ::("Array"),
    "symbolic dereferentiation of type vars works (1)";
}

{
  class A::B::C {}
  cmp_ok ::A::B::C, &infix:<=:=>, ::A::("B")::C,
    "symbolic dereferentiation of type vars works (2)";
}
