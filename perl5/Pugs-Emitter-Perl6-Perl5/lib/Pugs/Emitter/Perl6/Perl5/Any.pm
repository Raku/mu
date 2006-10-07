package Pugs::Emitter::Perl6::Perl5::Any;

# Compile-time a Perl 5 thing, with hardcoded, autoboxed  methods

use strict;
use warnings;
use base 'Pugs::Emitter::Perl6::Perl5::node';

sub name {
    $_[0]->{name}
}

sub WHAT { 
    $_[0]->node( 'Str', 'Any' );
}

sub isa { 
    return $_[0]->WHAT . ' eq ' . $_[1]->WHAT;
}

sub get {
    my $self = $_[0];
    return $self->name;
}

sub set {
    my $self = $_[0];
    return $self->name . ' = ' . $_[1]->get;
}

sub str {
    $_[0]->node( 'StrExpression', '( "" . ' . $_[0] . ' )' );
}

sub perl {
    $_[0]->node( 'StrExpression', 'Pugs::Runtime::Perl6::Scalar::perl( ' . $_[0] . ' )' );
}
    
sub defined {
    'defined ' . $_[0];
}

sub print {
    $_[0]->node( 'AnyExpression', '( print ' . $_[0]->str . ' )' );
}

sub say {
    $_[0]->node( 'AnyExpression', '( print ' . $_[0]->str . ', "\n" )' );
}

sub warn {
    $_[0]->node( 'AnyExpression', '( warn ' . $_[0]->str . ' )' );
}

sub yaml {
    $_[0]->node( 'StrExpression', 'Pugs::Runtime::Perl6::Scalar::yaml( ' . $_[0] . ' )' );
}

1;


