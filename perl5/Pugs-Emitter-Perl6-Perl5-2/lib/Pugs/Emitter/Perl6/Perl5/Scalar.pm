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
        $_[0]->node( 'BoolExpression',
                'Pugs::Runtime::Perl6::Scalar::isa( '. $_[0] . ', ' . $_[1] . ')' );
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

sub true {
    return $_[0]->node( 'BoolExpression',  $_[0] . '->true' )
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

    sub list { 
        $_[0]->node( 'Seq', [ $_[0] ] );
    }

sub _123__125_ {
    # .{}
    my $self = $_[0];
    my $other = $_[1]->list;
    return $_[0] unless $other;  # TODO
    return $self->_dollar_name . '{' . $other . '}';
}
package Pugs::Emitter::Perl6::Perl5::Perl5Scalar;
    use base 'Pugs::Emitter::Perl6::Perl5::Scalar';
    use overload (
        '""'     => sub { 
            Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
        },
        fallback => 1,
    );
    sub value {
        return $_[0]->node( 'ValueScalar', $_[0]{name} )
    }
    sub my {
        return $_[0]->node( 'MyScalar', $_[0]{name} )
    }
package Pugs::Emitter::Perl6::Perl5::MyScalar;
    use base 'Pugs::Emitter::Perl6::Perl5::Scalar';
    use overload (
        '""'     => sub { 
            'my ' . Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
        },
        fallback => 1,
    );
    sub set {
        $_[0] . ' = \( my $' . $_[0]->new_id . ' = ' . $_[1] . ')'
    }
package Pugs::Emitter::Perl6::Perl5::ValueScalar;
    use base 'Pugs::Emitter::Perl6::Perl5::Scalar';
    use overload (
        '""'     => sub { 
            '$' . Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
        },
        fallback => 1,
    );

1;

__END__

            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }

        
