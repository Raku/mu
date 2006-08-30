package Pugs::Emitter::Perl6::Perl5::Perl5Scalar;

# Compile-time Perl 5 scalar object - hardcoded, autoboxed  methods

use Data::Dumper;
use strict;
use warnings;

sub other_get {
    package Pugs::Emitter::Perl6::Perl5;
    use Data::Dumper;
    my $s = _emit( $_[1] );
    print "Scalar ", Dumper( $_[1] );
    if ( blessed $_[1] ) {
        return $_[1]->scalar;
    }
    $s;
}

sub new {
    my $self = $_[1];  # { name => '$scalar5' }
    bless $self, $_[0];
    return $self;
}

sub name {
    $_[0]->{name}
}

sub ref { 
    return "'Scalar'";  # ??? 
}

sub isa { 
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Scalar'";  # hardcoded 
}

sub get {
    my $self = $_[0];
    return $self->name;
}

sub set {
    my $self = $_[0];
    print "perl5scalar set ", Dumper( $_[1] );
    return $self->name . ' = ' . $self->other_get( $_[1] )->name;
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
