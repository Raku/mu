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

=cut

my $hash = {
   '1' =>  { '2' => 3,  '4' => 5}
   };


plan 2;
is( $hash<1><2>, '3', 'First nested element.');
is( $hash<1><4>, '5', 'Second nested element.', :todo<bug>);
