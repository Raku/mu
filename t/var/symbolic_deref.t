#!/usr/bin/pugs

use v6;
use Test;

plan 22;

# See http://www.nntp.perl.org/group/perl.perl6.language/22858 --
# previously, "my $a; say $::("a")" died (you had to s/my/our/). Now, it was
# re-specced to work.

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
  my $result;

  try {
    my $a_var = 42;
    my $sub   = sub { $::("CALLER")::("a_var") };
    $result = $sub();
  };

  is $result, 42, "symbolic dereferentation works with ::CALLER, too";
}

# Symbolic dereferentiation of Unicode vars (test primarily aimed at PIL2JS)
{
  my $äöü = 42;
  is $::("äöü"), 42, "symbolic dereferentiation of Unicode vars works";
}

=pod

Following re-specced to be invalid:

# Symbolic dereferentiation of lexical vars should *not* work without using
# $MY::foo:
{
  my $a_var = 42;

  dies_ok { $::("a_var") },
    "symbolic dereferentiation does not work for lexicals", :todo<bug>;
  is      $::("MY::a_var"), 42,
    "symbolic dereferentiation does work for lexicals when using MY::", :todo<bug>;
}

=cut

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
  my $ok;
  eval '
    class A::B::C {}
    $ok = ::A::B::C =:= ::A::("B")::C;
  ';
  ok $ok, "symbolic dereferentiation of (own) type vars works (2)";
}

# Symbolic dereferentiation syntax should work with $?SPECIAL etc. too.
# Note: I'm not 100% sure this is legal syntax. If it turns out it isn't, we'll
# have to s/ok/dies_ok/.
{
  try { this_will_die_and_therefore_set_dollar_exclamation_mark };
  ok $::("!"),    "symbolic dereferentiation works with special chars (1)";
  ok $::!,        "symbolic dereferentiation works with special chars (2)";
  ok %::("*ENV"), "symbolic dereferentiation works with special chars (3)";
  ok %::*ENV,     "symbolic dereferentiation works with special chars (4)";
}

# Symdereffing should find package vars as well:
{
  our $symderef_test_var = 42;

  is $::("symderef_test_var"), 42, "symbolic dereferentiation works with package vars";
}
