#!/usr/bin/pugs

use v6;
require Test;

plan 1;

=pod

This is a bug in how \ are parsed in certain 
single quoted strings can produce odd errors

=cut

{
    my $foo;
    eval "$foo = 'test\\'"; # this is eval-ing 'test\'
    is($foo, "test\\", '... string escaped correctly');
}