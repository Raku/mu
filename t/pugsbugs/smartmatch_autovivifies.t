#!/usr/bin/pugs

use v6;
use Test;

plan 3; 
=pod

Testing a non-existent array element for a match creates the 
array element

=cut

my @empty ;
is +@empty,0, 'Sanity: empty array, @empty, has 0 elements'; 

my $before =  @empty.perl;
@empty[5] ~~ /nothing/;
my $after = @empty.perl;

is +@empty,0,'empty array, @empty, has 0 elements'; 

is $after,$before,"Array elements are not auto-vivified by smartmatch";

