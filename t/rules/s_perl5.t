#!/usr/bin/pugs

use v6;
require Test;

plan 3;

=pod

Really really really minimal s:perl5//// tests.

Please add more!!

=cut

my $foo = "foo"; 
$foo ~~ s:perl5{f}{b}; 
is($foo, "boo", 'substitute regexp works');

my $bar = "barrrr"; 
$bar ~~ s:perl5:g{r+}{z}; 
is($bar, "baz", 'substitute regexp works with :g modifier');

my $path = "/path//to///a//////file"; 
$path ~~ s:perl5:g{/+}{/}; 
is($path, "/path/to/a/file", 'substitute regexp works with :g modifier');