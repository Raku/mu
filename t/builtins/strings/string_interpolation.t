#!/usr/bin/pugs

use v6;
use Test;

=kwid

=head1 String interpolation

These tests derived from comments in http://use.perl.org/~autrijus/journal/23398

=cut

plan 19;

my $world = "World";
my @list  = (1,2);
my %hash  = (1=>2);
sub func { return "func-y town" }

# Double quotes
is("Hello $world", 'Hello World', 'double quoted string interpolation works');
is("@list[]\ 3 4", '1 2 3 4', 'double quoted list interpolation works');
is("@list 3 4", '@list 3 4', 'array without empty square brackets does not interpolate');
is("%hash{}", "1\t2\n", 'hash interpolation works');
is("%hash", '%hash', 'hash interpolation does not work if not followed by {}');
is("Wont you take me to &func()", 'Wont you take me to func-y town', 'closure interpolation');
is("2 + 2 = { 2+2 }", '2 + 2 = 4', 'double quoted closure interpolation works');

# Single quotes
# XXX the next tests will always succeed even if '' interpolation is buggy
is('Hello $world', 'Hello $world', 'single quoted string interpolation does not work (which is correct)');
is('2 + 2 = { 2+2 }', '2 + 2 = { 2+2 }', 'single quoted closure interpolation does not work (which is correct)');
is('$world @list[] %hash{} &func()', '$world @list[] %hash{} &func()', 'single quoted string interpolation does not work (which is correct)');

# Corner-cases
is(eval('"Hello $world!"'), "Hello World!", "! is not a part of var names");
sub list_count (*@args) { +@args }
is(list_count("@list[]"), 1, 'quoted interpolation gets string context');
is(qq{a{chr 98}c}, 'abc', "curly brace delimiters don't interfere with closure interpolation", :todo<bug>);

# Quoting constructs
# The next test will always succeed, but if there's a bug it probably
# won't compile.
is(q0"abc\\d\\'\/", q0"abc\\d\\'\/", "raw quotation works");
is(q1"abc\\d\"\'\/", q0|abc\d"\'\/|, "single quotation works"); #"
is(q2"abc\\d\"\'\/", q0|abc\d"'/|, "double quotation works"); #"
is(qa"$world @list[] %hash{}", q0"$world 1 2 %hash{}", "only interpolate array");
is(qb"$world \\\"\n\t", "\$world \\\"\n\t", "only interpolate backslash");
is('$world \qq[@list[]] %hash{}', '$world 1 2 %hash{}', "interpolate quoting constructs in ''");
