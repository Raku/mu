package Pugs::Emitter::Perl6::Perl5::Perl5Array;

# Compile-time Perl 5 array object - hardcoded, autoboxed  methods

use strict;
use warnings;

sub other_get {
    package Pugs::Emitter::Perl6::Perl5;
    use Data::Dumper;
    print Dumper( $_[1] );
    _emit( $_[1] );
}

sub new {
    my $self = $_[1];  # { name => '%hash5' }
    bless $self, $_[0];
    return $self;
}

sub name {
    $_[0]->{name}
}

sub dollar_name {
    my $name = $_[0]->{name};
    $name =~ s/^\@/\$/;
    return $name;
}

sub ref { 
    return "'Array'";  # hardcoded 
}

sub isa { 
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Array'";  # hardcoded 
}

sub get {
    my $self = $_[0];
    return $self->name;
}

sub set {
    my $self = $_[0];
    return $self->name . ' = ' . $self->other_get( $_[1] );
}

sub str {
    # TODO
}

sub perl {
    # TODO
}
    
sub defined {
    'defined ' . $_[0]->dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub kv {
    my $tmp = "( map { ( \$_, ".$_[0]->name."[\$_] ) } 0..".$_[0]->name."-1 )"; 
    return ( ref $_[0] )->new( { name => $tmp } );
}

sub keys {
    my $tmp = "( 0..".$_[0]->name."-1 )"; 
    return ( ref $_[0] )->new( { name => $tmp } );
}

sub values {
    return $_[0]->name; 
} 

sub elems {
    'scalar ' . $_[0]->name;
}

sub exists {
    'exists ' . $_[0]->dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub delete {
    'delete ' . $_[0]->dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub hash {
    return Pugs::Emitter::Perl6::Perl5::Perl5Hash->new( {
        name => '%{{' . $_[0]->name . '}}' 
    } );
}

sub array {
    $_[0]->name;
}

sub _91__93_ {
    # .[]
    my $self = $_[0];
    my $other = $self->other_get( $_[1] );
    return $_[0]->name unless $other;  # TODO
    return $self->dollar_name . '[' . $other . ']';
}

1;


