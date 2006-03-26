#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Locale-KeyedText/t/lib>;

use Test;

plan( 4 );

use Locale::KeyedText; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Locale::KeyedText.meta.identifier.version, 1.72.2,
    'Locale::KeyedText is the correct version' );} );

use Locale::KeyedText::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Locale::KeyedText::L::en.meta.identifier.version, 1.0.1,
    'Locale::KeyedText::L::en is the correct version' );} );
