#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 7;

# use_ok( 'Perl6::Container::Array' );
use Perl6::Container::Hash; 
# use Perl6::Value;
# use Perl6::Value::List;

# use constant Inf => Perl6::Value::Num::Inf;

{
  my $h = { a => 1, b => 2 };
  
  is( Perl6::Container::Hash::elems( $h ), 2, 'hash elems' );
  
  my $h2 = Perl6::Container::Hash::clone( $h );
  is( ref( $h2 ), 'HASH', 'clone is a hashref' );
  
  delete $$h{'a'};
  delete $$h{'b'};
  is( Perl6::Container::Hash::elems( $h ), 0, 'empty hash' );

  is( Perl6::Container::Hash::elems( $h2 ), 2, 'cloned hash' );
  
  like( Perl6::Container::Hash::buckets( $h2 ), qr/\d+\/\d+/, 'buckets' );
}

{
    my $h = bless {}, 'Perl6::Container::Hash::Object';
    my $k = bless [1], 'obj1';
    my $v = bless [2], 'obj2';
    $h->store( $k, $v );
    my $x = $h->firstkey;
    my $y = $h->fetch( $x );
    is( ref($x), 'obj1', 'key is an object' );
    is( ref($y), 'obj2', 'value is an object' );
}
