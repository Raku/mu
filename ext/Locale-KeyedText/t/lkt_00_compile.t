#!pugs
use v6;

use Test;

plan( 6 ); # 12

use_ok( 'Locale::KeyedText-0.1.0' ); # bug: says OK even if version part is wrong

use lib;
BEGIN { import( 'lib': 't/lib' ); }

use_ok( 't_LKT_Util' ); # bug: fails to find in 'lib' despite 'use lib'
#can_ok( 't_LKT_Util', 'message' );
#can_ok( 't_LKT_Util', 'serialize' );

use_ok( 't_LKT_A_L_Eng' );
#can_ok( 't_LKT_A_L_Eng', 'get_text_by_key' );

use_ok( 't_LKT_A_L_Fre' );
#can_ok( 't_LKT_A_L_Fre', 'get_text_by_key' );

use_ok( 't_LKT_B_L_Eng' );
#can_ok( 't_LKT_B_L_Eng', 'get_text_by_key' );

use_ok( 't_LKT_B_L_Fre' );
#can_ok( 't_LKT_B_L_Fre', 'get_text_by_key' );
