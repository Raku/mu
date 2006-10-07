package Pugs::Emitter::Perl6::Perl5::Scalar;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value'; # XXX
    use overload (
        '""'     => sub { 
            $_[0]->{name} 
        },
        fallback => 1,
    );

    sub WHAT { 
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::ref( \\'. $_[0] . ')' );
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
    return $self->name . ' = ' . $_[1]->hash->get;
}

    sub perl {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::perl( '. $_[0] . ')' );
    }
    
    sub yaml {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::yaml( '. $_[0] . ')' );
    }
    sub str {
        return $_[0]->node( 'StrExpression', '( "" . ' . $_[0] . ' )' )
    }

sub defined {
    # TODO
}

sub kv {
    return $_[0]->node( 'ListExpression', $_[0] . '->kv' )   
}

sub keys {
    return $_[0]->node( 'ListExpression', $_[0] . '->keys' )   
}

sub num {
    return $_[0]->node( 'IntExpression',  $_[0] . '->num' )
} 

sub int {
    return $_[0]->node( 'IntExpression',  $_[0] . '->int' )
} 

sub elems {
    return $_[0]->node( 'IntExpression',  $_[0] . '->elems' )
}

sub hash {
    return $_[0]->node( 'HashExpression', $_[0] . '->hash' )
}

sub array {
    return $_[0]->node( 'ListExpression', $_[0] . '->array' )
}

sub scalar {
    return $_[0]
}

sub _123__125_ {
    # .{}
    my $self = $_[0];
    my $other = $_[1]->list;
    return $_[0] unless $other;  # TODO
    return $self->_dollar_name . '{' . $other . '}';
}

1;

__END__

            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }

        
