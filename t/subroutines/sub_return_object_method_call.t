#!/usr/bin/pugs

use v6;
use Test;

plan 10;


# These test calling the method ( or multi sub ) of a returned object


my $test;

my %suite = {
             number => 150,
             string => "string"
            };

sub init ( $i ) { $test = $i };
multi indirect1 ( $r ) { return $r };
multi indirect2 ( $r ) { return $r };


for %suite.kv -> $type, $value {
        ok( "Going to test" );

        init( $value );

        is $test.indirect1.indirect2, $test, 'Try with '~$type~': $object.foo.bar works';
        is $test.indirect1().indirect2, $test, 'Try with '~$type~': $object.foo().bar works';
        is $test.indirect1.indirect2(), $test, 'Try with '~$type~': $object.foo.bar() works';
        is $test.indirect1().indirect2(), $test, 'Try with '~$type~': $object.foo.bar() works';

        $test.undefine;
}

