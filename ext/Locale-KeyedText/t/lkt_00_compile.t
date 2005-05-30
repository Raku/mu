#!pugs
use v6;

use Test;

plan( 12 ); # was 13 in p5

use_ok( 'Locale::KeyedText-0.1.1' ); # bug: says OK even if version part is wrong

use lib <t/lib ext/Locale-KeyedText/t/lib>;

use_ok( 't_LKT_Util' );
skip( "can_ok( 't_LKT_Util', 'message' );" );
skip( "can_ok( 't_LKT_Util', 'serialize' );" );

use_ok( 't_LKT_A_L_Eng' );
skip( "can_ok( 't_LKT_A_L_Eng', 'get_text_by_key' );" );

use_ok( 't_LKT_A_L_Fre' );
skip( "can_ok( 't_LKT_A_L_Fre', 'get_text_by_key' );" );

use_ok( 't_LKT_B_L_Eng' );
skip( "can_ok( 't_LKT_B_L_Eng', 'get_text_by_key' );" );

use_ok( 't_LKT_B_L_Fre' );
skip( "can_ok( 't_LKT_B_L_Fre', 'get_text_by_key' );" );
