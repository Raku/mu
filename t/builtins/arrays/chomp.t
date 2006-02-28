#!/usr/bin/pugs

use v6;
use Test;

plan 9;

=pod

Basic tests for the chomp() builtin working on an array of strings

=cut

# L<S29/"Perl6::Str" /chomp/>
# Also see L<"http://use.perl.org/~autrijus/journal/25351">
#   &chomp and &wrap are now nondestructive; chomp returns the chomped part,
#   which can be defined by the filehandle that obtains the default string at
#   the first place. To get destructive behaviour, use the .= form.
# Since currently the behaviour with regards to arrays is not defined, I'm
# assuming the correct behaviour is an extension of the behaviour for
# a single string.

{
    my @foo = ("foo\n","bar\n","baz\n");
    chomp(@foo);
    is(@foo[0], "foo\n", '1st element was not yet chomped', :todo<feature>);
    is(@foo[1], "bar\n", '2nd element was not yet chomped', :todo<feature>);
    is(@foo[2], "baz\n", '3rd element was not yet chomped', :todo<feature>);
    @foo .= chomp;
    is(@foo[0], 'foo', '1st element chomped correctly', :todo<feature>);
    is(@foo[1], 'bar', '2nd element chomped correctly', :todo<feature>);
    is(@foo[2], 'baz', '3rd element chomped correctly', :todo<feature>);
    $foo .= chomp;
    is(@foo[0], 'foo', '1st element is chomped again with no effect', :todo<feature>);
    is(@foo[1], 'bar', '2nd element is chomped again with no effect', :todo<feature>);
    is(@foo[2], 'baz', '3rd element is chomped again with no effect', :todo<feature>);
}

