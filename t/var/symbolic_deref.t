#!/usr/bin/pugs

use v6;
use Test;

plan 22;

# L<S02/"Names and Variables" /All symbolic references are done with this notation:/>
{
  our $a_var = 42;
  my  $b_var = "a_var";

  is $::($b_var), 42, 'basic symbolic scalar dereferentiation works';
}

{
  our @a_var = <a b c>;
  my  $b_var = "a_var";

  is @::($b_var)[1], "b", 'basic symbolic array dereferentiation works';
}

{
  our %a_var = (a => 42);
  my  $b_var = "a_var";

  is %::($b_var)<a>, 42, 'basic symbolic hash dereferentiation works';
}

{
  our &a_var = { 42 };
  my  $b_var = "a_var";

  is &::($b_var)(), 42, 'basic symbolic code dereferentiation works';
}

{
  our $pugs::is::cool = 42;
  my  $cool = "cool";

  is $::("pugs")::is::($cool), 42, 'not so basic symbolic dereferentiation works';
}

{
  my $a_var = 42;
  my $sub   = sub { $::("CALLER")::("a_var") };
  is $sub(), 42, "symbolic dereferentation works with ::CALLER, too";
}

# Symbolic dereferentiation of lexical vars should *not* work without using
# $MY::foo:
{
  my $a_var = 42;

  dies_ok { $::("a_var") },
    "symbolic dereferentiation does not work for lexicals", :todo<bug>;
  is      $::("MY::a_var"),
    "symbolic dereferentiation does work for lexicals when using MY::", :todo<bug>;
}

# Symbolic dereferentiation of globals
{
  sub *a_global_sub () { 42 }
  is &::("*::a_global_sub")(), 42,
    "symbolic dereferentiation of globals works (1)", :todo<bug>;

  our $*a_global_var = 42;
  is $::("*::a_global_var"),   42,
    "symbolic dereferentiation of globals works (2)", :todo<bug>;
}

# Symbolic dereferentiation of globals *without the star*
{
  cmp_ok $::("*IN"), &infix:<=:=>, $*IN,
    "symbolic dereferentiation of globals works (3)";
  cmp_ok $::("IN"),  &infix:<=:=>, $*IN,
    "symbolic dereferentiation of globals without the star works";

  # XXX - should be =:= rather than ~~, but &say =:= &say is currently false.:(
  #cmp_ok &::("*say"), &infix:<=:=>, &say,
  cmp_ok &::("*say"), &infix:<~~>, &say,
    "symbolic dereferentiation of global subs works";
  #cmp_ok &::("say"),  &infix:<=:=>, &say,
  cmp_ok &::("say"),  &infix:<~~>, &say,
    "symbolic dereferentiation of global subs without the star works (1)";

  ok &::("true")(42),
    "symbolic dereferentiation of global subs without the star works (2)";
  is &::("int")(3.1), 3,
    "symbolic dereferentiation of global subs without the star works (3)";
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

# Symbolic dereferentiation syntax should work with $?SPECIAL etc. too.
# Note: I'm not 100% sure this is legal syntax. If it turns out it isn't, we'll
# have to s/ok/dies_ok/.
{
  eval 'this_will_die_and_therefore_set_$!';
  ok $::("!"),    "symbolic dereferentiation works with special chars (1)";
  ok $::!,        "symbolic dereferentiation works with special chars (2)";
  ok %::("*ENV"), "symbolic dereferentiation works with special chars (3)";
  ok %::*ENV,     "symbolic dereferentiation works with special chars (4)";
}
