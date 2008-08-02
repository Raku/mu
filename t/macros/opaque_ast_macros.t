use v6;

use Test;

=begin pod
=head1 DESCRIPTION

Tests for macros which return q:code but do not do splicing

See L<S06/"Macros">.

=end pod

plan 5;

# L<S06/Macros>
macro four () { q:code { 2+2 } } 

is(four, 4, "macro returning q:code");

macro hi () { q:code(:COMPILING) { "hello $s" } } 

macro hey () { ({ "hello $^s" }.body) } 

my $s="world"; 
is(hi(),"hello world","macros can bind in caller's lexical env");

$s="paradise"; 
is(hi(),"hello paradise","macros but it's a binding only");
is(hey(),"hello paradise","macros but it's a binding only");

my $x;
macro noop ()  { $x = "Nothing happened"; q:code { } } 
noop();
#macro noop2 () { $x ~= ", twice"; return } # unspecced
#noop2();

is($x,"Nothing happened", "Macros can return noops");
