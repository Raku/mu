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
    # TODO
}

sub perl {
    # TODO
}
    
sub defined {
    'defined ' . $_[0];
}

sub kv {
    my $tmp = "( map { ( \$_, ".$_[0]->name."[\$_] ) } 0..".$_[0]->name."-1 )"; 
    return ( CORE::ref( $_[0] ) )->new( { name => $tmp } );
}

sub keys {
    my $tmp = "( 0..".$_[0]->name."-1 )"; 
    return ( CORE::ref( $_[0] ) )->new( { name => $tmp } );
}

sub values {
    return $_[0]->name; 
} 

sub elems {
    'scalar ' . $_[0]->name;
}

sub exists {
    'exists ' . $_[0] . '[' . $_[1] . ']';
}

sub delete {
    'delete ' . $_[0] . '[' . $_[1] . ']';
}

sub hash {
    $_[0]->node( 'Perl5Hash', '%{{' . $_[0]->name . '}}' );
}

sub array {
    $_[0]->name;
}

sub scalar {
    my $tmp = $_[0]->name;
    if ( $tmp =~ /^ \@\{ (\[  .*  \]) \} $/x ) {
        return $_[0]->node( 'Scalar', "bless $1, 'Pugs::Runtime::Perl6::Array'" );        
    }
    return $_[0]->node( 'Scalar', 'bless \\' . $_[0]->name . ", 'Pugs::Runtime::Perl6::Array'" );
}

sub _91__93_ {
    # .[]
    my $self = $_[0];
    my $other = $self->other_get( $_[1] );
    return $_[0] unless $other;  # TODO
    return $self . '[' . $other . ']';
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


