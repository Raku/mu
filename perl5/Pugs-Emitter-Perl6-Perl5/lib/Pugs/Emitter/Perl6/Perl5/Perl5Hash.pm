package Pugs::Emitter::Perl6::Perl5::Perl5Hash;

# Compile-time Perl 5 hash object - hardcoded, autoboxed  methods

use Data::Dumper;
use strict;
use warnings;

sub _dollar_name {
    my $name = $_[0]->{name};
    $name =~ s/\%/\$/;
    return $name;
}

sub WHAT { 
    $_[0]->node( 'Str', 'Hash' );
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
    print "perl5hash set ", Dumper( $_[1] );
    return $self->name . ' = ' . $_[1]->hash->get;
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

sub keys {
    return Pugs::Emitter::Perl6::Perl5::Perl5Array->new( {
        name => '(keys ' . $_[0]->name . ')'
    } );    
}

sub elems {
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => 'scalar keys ' . $_[0]->name
    } );
}

sub hash {
    $_[0]->name;
}

sub array {
    return Pugs::Emitter::Perl6::Perl5::Perl5Array->new( {
        name => '@{[' . $_[0]->name . ']}'
    } );    
}

sub scalar {
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => 'bless \\' . $_[0]->name . ", 'Pugs::Runtime::Perl6::Hash'" 
    } );
}

sub _123__125_ {
    # .{}
    my $self = $_[0];
    my $other = $_[1]->list;
    return $_[0] unless $other;  # TODO
    return $self->_dollar_name . '{' . $other . '}';
}

1;
