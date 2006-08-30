package Pugs::Emitter::Perl6::Perl5::Str;

# Compile-time Perl 5 hash object - hardcoded, autoboxed  methods

use Data::Dumper;
use strict;
use warnings;

sub new {
    my $self = $_[1];  # { value => 'x' }
    bless $self, $_[0];
    return $self;
}

sub ref { 
    return "'Str'";  # hardcoded 
}

sub isa { 
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Str'";  # hardcoded 
}

sub str {
    $_[0]
}

sub perl {
    "'" . $_[0]->{value} . "'"
}
    
sub name {
    $_[0]->perl
}
    
sub scalar {
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => 'bless \\' . $_[0]->perl . ", 'Pugs::Runtime::Perl6::Str'" 
    } );
}

1;
