#!/usr/bin/pugs

use v6;
require Test;

=kwid

Quoting tests

These tests are derived from Synopsis 2 and 
only cover the following quoting operators:

q//, q:1//, q:single//
qq//, q:2//, q:double//
qw//, q:w//, q:words//

=cut

plan 33;

# some basic quote (non-interpolated)

my @quote1a;
eval '@quote1a = q/$foo $bar/';
todo_is(+@quote1a, 2, 'q// returns the correct amount of elements');
todo_is(@quote1a[0], '$foo', 'first element of q// is correct');
todo_is(@quote1a[1], '$bar', 'second element of q// is correct');

# ... with the new quote operators

my @quote1b;
eval '@quote1b = q:1/$foo $bar/';
todo_is(+@quote1b, 2, 'q:1// returns the correct amount of elements');
todo_is(@quote1b[0], '$foo', 'first element of q:1// is correct');
todo_is(@quote1b[1], '$bar', 'second element of q:1// is correct');

my @quote1c;
eval '@quote1c = q:single/$foo $bar/';
todo_is(+@quote1c, 2, 'q:single// returns the correct amount of elements');
todo_is(@quote1c[0], '$foo', 'first element of q:single// is correct');
todo_is(@quote1c[1], '$bar', 'second element of q:single// is correct');

# some basic quote word stuff

my @quote2a = qw/foo bar/;
is(+@quote2a, 2, 'qw// returns the correct amount of elements');
is(@quote2a[0], 'foo', 'first element of qw// is correct');
is(@quote2a[1], 'bar', 'second element of qw// is correct');

my @quote2b = qw/"foo" "bar"/;
is(+@quote2b, 2, 'qw// returns the correct amount of elements');
is(@quote2b[0], '"foo"', 'first element of qw// is correct');
is(@quote2b[1], '"bar"', 'second element of qw// is correct');

my @quote2c = qw/"foo, :bar"/;
is(+@quote2c, 2, 'qw// returns the correct amount of elements');
is(@quote2c[0], '"foo,', 'first element of qw// is correct');
is(@quote2c[1], ':bar"', 'second element of qw// is correct');

# ... with the new quote operators

my @quote2d;
eval '@quote2d = q:w/foo bar/';
todo_is(+@quote2d, 2, 'q:w// returns the correct amount of elements');
todo_is(@quote2d[0], 'foo', 'first element of q:w// is correct');
todo_is(@quote2d[1], 'bar', 'second element of q:w// is correct');

my @quote2e;
eval '@quote2e = q:words/foo bar/';
todo_is(+@quote2e, 2, 'q:words// returns the correct amount of elements');
todo_is(@quote2e[0], 'foo', 'first element of q:words// is correct');
todo_is(@quote2e[1], 'bar', 'second element of q:words// is correct');

# some basic quote (interpolated)

my $foo = 'FOO';
my $bar = 'BAR';

my @quote3a;
eval '@quote3a = qq/$foo $bar/';
todo_is(+@quote3a, 2, 'qq// returns the correct amount of elements');
todo_is(@quote3a[0], 'FOO', 'first element of qq// is correct');
todo_is(@quote3a[1], 'BAR', 'second element of qq// is correct');

# ... with the new quote operators

my @quote3b;
eval '@quote3b = q:2/$foo $bar/';
todo_is(+@quote3b, 2, 'q:2// returns the correct amount of elements');
todo_is(@quote3b[0], 'FOO', 'first element of q:2// is correct');
todo_is(@quote3b[1], 'BAR', 'second element of q:2// is correct');

my @quote3c;
eval '@quote3c = q:double/$foo $bar/';
todo_is(+@quote3c, 2, 'q:double// returns the correct amount of elements');
todo_is(@quote3c[0], 'FOO', 'first element of q:double// is correct');
todo_is(@quote3c[1], 'BAR', 'second element of q:double// is correct');

