#!/usr/bin/pugs

use v6;
require Test;

plan 10;

=pod

Basic tests for the chomp() builtin

=cut

{
    my $foo = "foo\n";
    chomp($foo);
    is($foo, 'foo', 'our variable is chomped correctly');
    chomp($foo);
    is($foo, 'foo', 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\n\n";
    chomp($foo);
    is($foo, "foo\n", 'our variable is chomped correctly');
    chomp($foo);
    is($foo, 'foo', 'our variable is chomped again correctly');
    chomp($foo);
    is($foo, 'foo', 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\nbar\n";
    chomp($foo);
    is($foo, "foo\nbar", 'our variable is chomped correctly');
    chomp($foo);
    is($foo, "foo\nbar", 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\n ";
    chomp($foo);
    is($foo, "foo\n ", 'our variable is chomped with no effect');
}

{
    my $foo = "foo\n";
    my $chomped_foo = chomp($foo);
    is($chomped_foo, "\n", 'chomp returns the chomped value');
    is($foo, 'foo', 'and our variable is chomped correctly');    
}

