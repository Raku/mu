use v6-alpha;

use lib <t/lib ext/Locale-KeyedText/t/lib>;

use Test;

plan( 4 );

use Locale::KeyedText; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Locale::KeyedText.WHO.version, 1.73.1,
    'Locale::KeyedText is the correct version' );} );

use Locale::KeyedText::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Locale::KeyedText::L::en.WHO.version, 1.0.2,
    'Locale::KeyedText::L::en is the correct version' );} );
