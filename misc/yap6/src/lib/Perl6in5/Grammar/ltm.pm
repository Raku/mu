package Perl6in5::Grammar;
use Perl6in5::Grammar;

pattern program {
    nthru( foo, eoi )
};

pattern foo {
      lit( 'foo' )
    | lit( 'foodie' ) . unmore( 's' )
    | lit( 'food' )                          # foodies should match here
    | lit( 'fo' )
};