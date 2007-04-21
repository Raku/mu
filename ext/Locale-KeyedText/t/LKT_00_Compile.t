use v6-alpha;

use Test;

plan( 6 );

use_ok( 'Locale::KeyedText' );
skip( 1, q{is( Locale::KeyedText.WHO.version, 1.74.0,
    'Locale::KeyedText is the correct version' );} );

use_ok( 'Locale::KeyedText::L::en' );
skip( 1, q{is( Locale::KeyedText::L::en.WHO.version, 1.0.3,
    'Locale::KeyedText::L::en is the correct version' );} );

use_ok( 'Locale::KeyedText::L::fr' );
skip( 1, q{is( Locale::KeyedText::L::fr.WHO.version, 1.0.0,
    'Locale::KeyedText::L::fr is the correct version' );} )
