#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 17;
 
use Perl6::Value;

is( Perl6::Value::Int::to_str( Perl6::Value::Str::to_int( 'NaN' )), 'NaN' );
is( Perl6::Value::Int::to_str( Perl6::Value::Str::to_int( '-Inf' )), '-Inf' );
is( Perl6::Value::Int::to_str( Perl6::Value::Str::to_int( 'Inf' )), 'Inf' );
is( Perl6::Value::Int::to_str( Perl6::Value::Str::to_int( '6.3' )), '6' );

is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( 'NaN' )), 'NaN' );
is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( '-Inf' )), '-Inf' );
is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( 'Inf' )), 'Inf' );
is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( '6.3' )), '6.3' );

is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( 'bool::true' )), 'bool::true' );
is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( '1' )), 'bool::true' );
is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( '0' )), 'bool::false' );
is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( '' )), 'bool::false' );

# other: space padded values, etc

is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( 'bool::false' )), 'bool::true' );
is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( ' 0' )), 'bool::true' );
is( Perl6::Value::Bit::to_str( Perl6::Value::Str::to_bit( ' 1' )), 'bool::true' );
is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( ' Inf' )), 'Inf' );
is( Perl6::Value::Num::to_str( Perl6::Value::Str::to_num( ' NaN' )), 'NaN' );
