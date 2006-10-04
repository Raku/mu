package Pugs::Emitter::Perl6::Perl5::Perl5Scalar;

# Compile-time Perl 5 scalar object - hardcoded, autoboxed  methods

use Data::Dumper;
use strict;
use warnings;

sub WHAT { 
    # XXX depends on the contents
    $_[0]->node( 'Str', 'Scalar' );
}

sub isa { 
    my $self = $_[0];
    return $_[0]->WHAT->eq( $_[1]->WHAT ); 
}

sub get {
    my $self = $_[0];
    return $self->name;
}

sub set {
    my $self = $_[0];
    print "perl5scalar set ", Dumper( $_[1] );
    return $self->name . ' = ' . $_[1]->scalar->get;
}

sub str {
    $_[0]
}

sub perl {
    $_[0] # TODO
}
    
sub defined {
    'defined ' . $_[0]
}

sub kv {
    # TODO
}

sub elems {
    '1';    # ???
}

sub scalar {
    $_[0]
}

1;
