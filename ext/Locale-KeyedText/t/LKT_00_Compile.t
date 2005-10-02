#!/usr/bin/pugs
use v6;

use Test;

plan( 13 );

use_ok( 'Locale::KeyedText' );
skip( q{is( Locale::KeyedText.meta.identifier.version, 1.6.2, 'Locale::KeyedText is the correct version' );} );

use lib <t/lib ext/Locale-KeyedText/t/lib>;

use_ok( 't_LKT_Util' );
skip( q{can_ok( 't_LKT_Util', 'message' );} );
skip( q{can_ok( 't_LKT_Util', 'serialize' );} );

use_ok( 't_LKT_A_L_Eng' );
skip( q{can_ok( 't_LKT_A_L_Eng', 'get_text_by_key' );} );

use_ok( 't_LKT_A_L_Fre' );
skip( q{can_ok( 't_LKT_A_L_Fre', 'get_text_by_key' );} );

use_ok( 't_LKT_B_L_Eng' );
skip( q{can_ok( 't_LKT_B_L_Eng', 'get_text_by_key' );} );

use_ok( 't_LKT_B_L_Fre' );
skip( q{can_ok( 't_LKT_B_L_Fre', 'get_text_by_key' );} );
