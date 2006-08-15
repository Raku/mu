#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 17;
 
use Pugs::Runtime::Value;

is( Pugs::Runtime::Value::Int::to_str( Pugs::Runtime::Value::Str::to_int( 'NaN' )), 'NaN' );
is( Pugs::Runtime::Value::Int::to_str( Pugs::Runtime::Value::Str::to_int( '-Inf' )), '-Inf' );
is( Pugs::Runtime::Value::Int::to_str( Pugs::Runtime::Value::Str::to_int( 'Inf' )), 'Inf' );
is( Pugs::Runtime::Value::Int::to_str( Pugs::Runtime::Value::Str::to_int( '6.3' )), '6' );

is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( 'NaN' )), 'NaN' );
is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( '-Inf' )), '-Inf' );
is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( 'Inf' )), 'Inf' );
is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( '6.3' )), '6.3' );

is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( 'bool::true' )), bool::true );
is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( '1' )), bool::true );
is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( '0' )), bool::false );
is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( ''  )), bool::false );

# other: space padded values, etc

is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( 'bool::false' )), bool::true );
is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( ' 0' )), bool::true );
is( Pugs::Runtime::Value::Bit::to_str( Pugs::Runtime::Value::Str::to_bit( ' 1' )), bool::true );
is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( ' Inf' )), 'Inf' );
is( Pugs::Runtime::Value::Num::to_str( Pugs::Runtime::Value::Str::to_num( ' NaN' )), 'NaN' );
