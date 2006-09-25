use Test::More tests => 2;
use Data::Dumper;

use Pugs::Emitter::Perl6::Perl5::Value;

sub node {
    eval 'use Pugs::Emitter::Perl6::Perl5::' . $_[0];
    ( 'Pugs::Emitter::Perl6::Perl5::' . $_[0] )->new( { name => $_[1] } );
}

{
    my $b = node( 'Bool', 1 );
    #print Dumper( $b );
    is( "$b", 1, 'emit bool' );
    is( "" . $b->not, 0, 'emit bool.not' );
}
