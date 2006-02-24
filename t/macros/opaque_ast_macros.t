#!/usr/bin/pugs
use v6;
use Test;

=head1 DESCRIPTION

Tests for macros which return CODE but do not do splicing

See L<S06/"Macros">.

=cut

plan 3;

macro four () { CODE { 2+2 } } 

is(four, 4, "macro returning CODE");

macro hi () { CODE { "hello "~$s } } 

my $s="world"; 
is(hi(),"hello world","macros can bind in caller's lexical env");

$s="paradise"; 
is(hi(),"hello paradise","macros but it's a binding only");
