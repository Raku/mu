use v6-alpha;

use Test;

plan 2;

my $value_from_BUILD;

class Foo {
    submethod BUILD (Str :$value) {
        $value_from_BUILD = $value;
    }
}

my $obj = Foo.new( value => 'bar' );

is( $value_from_BUILD, 'bar', 
    'BUILD arg declared as named and invoked with literal pair should'
    ~ ' contain only the pair value' );
is( $value_from_BUILD.WHAT, 'Str', 'same arg should be of declared type' );
