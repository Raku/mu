package Pugs::Emitter::Perl6::Perl5::Perl5Array;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value'; # XXX
    use overload (
        '""'     => sub { 
            Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
        },
        fallback => 1,
    );

    sub WHAT { 
        $_[0]->node( 'str', 'Array' );
    }

sub _dollar_name {
    my $name = $_[0]->{name};
    $name =~ s/^\@/\$/;
    return $name;
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
    # XXX box
    return $self->name . ' = ' . $_[1]->array->get;
}

sub str {
    # TODO
}

sub perl {
    # TODO
}
    
sub defined {
    # XXX move to BoolExpression
    'defined ' . $_[0]->_dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub kv {
    # XXX move to ArrayExpression
    my $tmp = "( map { ( \$_, ".$_[0]->name."[\$_] ) } 0..".$_[0]->name."-1 )"; 
    return ( CORE::ref( $_[0] ) )->new( { name => $tmp } );
}

sub keys {
    # XXX move to ArrayExpression
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
    'exists ' . $_[0]->_dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub delete {
    'delete ' . $_[0]->_dollar_name . '[' . $_[0]->other_get( $_[1] ) . ']';
}

sub hash {
    # XXX HashExpression
    return Pugs::Emitter::Perl6::Perl5::Perl5Hash->new( {
        name => '%{{' . $_[0]->name . '}}' 
    } );
}

sub array {
    $_[0]->name;
}

sub scalar {
    my $tmp = $_[0]->name;

    # XXX move to ArrayExpression
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
    my $other = $_[1]->list;
    return $_[0] unless $other;  # TODO
    return $self->_dollar_name . '[' . $other . ']';
}

1;


