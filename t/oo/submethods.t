#!/usr/bin/pugs

use v6;
require Test;

plan 10;

=pod

Basic submethod tests. See L<S12/"Submethods">

=cut

my $was_in_foo_build = 0;
my $was_in_bar_build = 0;

todo_eval_ok '
  class Foo         { submethod BUILD() { $was_in_foo_build++ } }
  class Bar isa Foo { submethod BUILD() { $was_in_bar_build++ } }
', "class definitions were parsed/run/compiled";

todo_eval_ok 'my $a = Foo.new()',  "Foo.new() worked (1)";
todo_is      $was_in_foo_build, 1, "Foo's BUILD was called";
# is instead of todo_is to avoid unexpected succeedings
is           $was_in_bar_build, 0, "Bar's BUILD was not called";

todo_eval_ok 'my $b = Bar.new()',  "Bar.new() worked";
todo_is      $was_in_foo_build, 2, "Foo's BUILD was called again";
todo_is      $was_in_bar_build, 1, "Bar's BUILD was called, too";

# The next three tests are basically exactly the same as the first three tests
# (not counting the initial class definition). This is to verify our call to
# Bar.new didn't removed/changed some internal structures which'd prevent
# Foo.BUILD of getting called.
todo_eval_ok 'my $c = Foo.new()',  "Foo.new() worked (2)";
todo_is      $was_in_foo_build, 3, "Foo's BUILD was called again";
todo_is      $was_in_bar_build, 1, "Bar's BUILD was not called again";
