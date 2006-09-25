package Pugs::Emitter::Perl6::Perl5::Any;

# Compile-time a Perl 5 thing, with hardcoded, autoboxed  methods

use strict;
use warnings;

sub other_get {
    Pugs::Emitter::Perl6::Perl5::_emit( $_[1] );
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

sub WHAT { 
    return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Any' } );
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
    'defined ' . $_[0]->dollar_name;
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

sub scalar {
    my $tmp = $_[0]->name;
    if ( $tmp =~ /^ \@\{ (\[  .*  \]) \} $/x ) {
        return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
            name => "bless $1, 'Pugs::Runtime::Perl6::Array'" 
        } );        
    }
    return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
        name => 'bless \\' . $_[0]->name . ", 'Pugs::Runtime::Perl6::Array'" 
    } );
}

sub _91__93_ {
    # .[]
    my $self = $_[0];
    my $other = $self->other_get( $_[1] );
    return $_[0] unless $other;  # TODO
    return $self->dollar_name . '[' . $other . ']';
}

sub print {
    return Pugs::Emitter::Perl6::Perl5::Code->new( {
        name => 'print ' . $_[0]->str
    } );
}

sub say {
    return Pugs::Emitter::Perl6::Perl5::Code->new( {
        name => 'print ' . $_[0]->str . ', "\n";'
    } );
}

1;


