#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 7;

# use_ok( 'Pugs::Runtime::Container::Array' );
use Pugs::Runtime::Container::Hash; 
# use Pugs::Runtime::Value;
# use Pugs::Runtime::Value::List;

# use constant Inf => Pugs::Runtime::Value::Num::Inf;

{
  my $h = { a => 1, b => 2 };
  
  is( Pugs::Runtime::Container::Hash::elems( $h ), 2, 'hash elems' );
  
  my $h2 = Pugs::Runtime::Container::Hash::clone( $h );
  is( ref( $h2 ), 'HASH', 'clone is a hashref' );
  
  delete $$h{'a'};
  delete $$h{'b'};
  is( Pugs::Runtime::Container::Hash::elems( $h ), 0, 'empty hash' );

  is( Pugs::Runtime::Container::Hash::elems( $h2 ), 2, 'cloned hash' );
  
  like( Pugs::Runtime::Container::Hash::buckets( $h2 ), qr/\d+\/\d+/, 'buckets' );
}

{
    my $h = bless {}, 'Pugs::Runtime::Container::Hash::Object';
    my $k = bless [1], 'obj1';
    my $v = bless [2], 'obj2';
    $h->store( $k, $v );
    my $x = $h->firstkey;
    my $y = $h->fetch( $x );
    is( ref($x), 'obj1', 'key is an object' );
    is( ref($y), 'obj2', 'value is an object' );
}
