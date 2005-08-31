#!/usr/bin/pugs

use v6;
use Test;

=pod

The parser has difficulties with nested hash refs.
my $hash = {
   '1' =>  { '2' => 3,  '4' => 5}
   };
 
say "Perl: ", $hash.perl;
say "Ref: ", $hash.ref;
say '$hash<1>.ref = ', $hash<1>.perl;
say '$hash<1><2>.ref = ', $hash<1><2>.ref;
say '$hash<1><4>.ref = ', $hash<1><4>.ref;
say '$hash<1><4>.ref = ', $hash<1><4>.perl;

Also with array refs nested in hash refs.

=cut

plan 4;

my $hash = {
   '1' =>  { '2' => 3,  '4' => 5}
   };


is( $hash<1><2>, '3', 'First nested element.');
is( $hash<1><4>, '5', 'Second nested element.');

my $h2 = {
   x => [2,3]
};
is( $h2<x>[0], '2', 'First nested element.');
is( $h2<x>[1], '3', 'Second nested element.');
