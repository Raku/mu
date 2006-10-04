package Pugs::Emitter::Perl6::Perl5::Pair;

# Compile-time Perl 5 hash object - hardcoded, autoboxed  methods

use Data::Dumper;
use strict;
use warnings;

sub WHAT { 
    return "'Pair'";  # hardcoded 
}

sub isa { 
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Pair'";  # hardcoded 
}

sub key {
    $_[0]->{key};
}

sub value {
    $_[0]->{value};
}

sub str {
    # TODO
}

sub perl {
    # TODO
}
    
sub defined {
    # TODO
}

sub kv {
    $_[0]->array
}

sub elems {
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => '1'
    } );
}

sub hash {
    # TODO
}

sub array {
    return Pugs::Emitter::Perl6::Perl5::Perl5Array->new( {
        name => '(' . $_[0]->key->name . ',' . $_[0]->value->name . ')'
    } );    
}

sub scalar {
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => 'bless {' . $_[0]->key->name . '=>' . $_[0]->value->name . "}, 'Pugs::Runtime::Perl6::Pair'" 
    } );
}

sub _123__125_ {
    # .{}
    my $self = $_[0];
    my $other = $self->other_get( $_[1] );
    return $_[0] unless $other;  # TODO
    return $self->dollar_name . '{' . $other . '}';
}

1;
