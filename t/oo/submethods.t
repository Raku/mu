#!/usr/bin/pugs

use v6;
use Test;

plan 16;

=pod

Basic submethod tests. See L<S12/"Submethods">

=cut

{
  my $was_in_foo_build = 0;
  my $was_in_bar_build = 0;

  eval_ok '
    class Foo        { submethod BUILD() { $was_in_foo_build++ } }
    class Bar is Foo { submethod BUILD() { $was_in_bar_build++ } }
  ', "class definitions were parsed/run/compiled", :todo<feature>;

  my $a;
  ok eval('$a = Foo.new()'),    "Foo.new() worked (1)", :todo<feature>;
  is $was_in_foo_build, 1,      "Foo's BUILD was called", :todo<feature>;
  # is instead of todo_is to avoid unexpected succeedings
  is $was_in_bar_build, 0,      "Bar's BUILD was not called";

  ok eval('$b = Bar.new()'),    "Bar.new() worked", :todo<feature>;
  is $was_in_foo_build, 2,      "Foo's BUILD was called again", :todo<feature>;
  is $was_in_bar_build, 1,      "Bar's BUILD was called, too", :todo<feature>;

  # The next three tests are basically exactly the same as the first three tests
  # (not counting the initial class definition). This is to verify our call to
  # Bar.new didn't removed/changed some internal structures which'd prevent
  # Foo.BUILD of getting called.
  my $c;
  ok eval('my $c = Foo.new()'), "Foo.new() worked (2)", :todo<feature>;
  is $was_in_foo_build, 3,      "Foo's BUILD was called again", :todo<feature>;
  is $was_in_bar_build, 1,      "Bar's BUILD was not called again", :todo<feature>;
}

{
  my $was_in_baz_submethod  = 0;
  my $was_in_grtz_submethod = 0;

  eval '
    class Baz         { submethod blarb() { $was_in_baz_submethod++ } }
    class Grtz is Baz { submethod blarb() { $was_in_grtz_submethod++ } }
  ';

  my ($baz, $grtz);
  ok eval('$baz  = Baz.new'),  "Baz.new() worked",  :todo<feature>;
  ok eval('$grtz = Grtz.new'), "Grtz.new() worked", :todo<feature>;

  try { $baz.blarb }
  is $was_in_baz_submethod,  1, "Baz's submethod blarb was called", :todo<feature>;
  # No :todo to avoid unexpected suceedings
  is $was_in_grtz_submethod, 0, "Grtz's submethod blarb was not called";

  try { $grtz.blarb }
  is $was_in_baz_submethod,  2, "Baz's submethod blarb was called again", :todo<feature>;
  # No :todo to avoid unexpected suceedings
  is $was_in_grtz_submethod, 1, "Grtz's submethod blarb was now called, too", :todo<feature>;
}
