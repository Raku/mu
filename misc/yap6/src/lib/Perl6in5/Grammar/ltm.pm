package Perl6in5::Grammar;
use Perl6in5::Grammar;

rule program {
    nthru( foo, eoi )
};

rule foo {
      lit( 'foo' )
    | lit( 'foodie' ) . unmore( 's' )
    | lit( 'food' )                          # foodies should match here
    | lit( 'fo' )
};