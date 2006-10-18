package Pugs::Emitter::Perl6::Perl5::Scalar;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value'; # XXX
    use overload (
        '""'     => sub { 
            '${' . $_[0]->name . '}'
        },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::ref( '. $_[0]->bind_from . ')' );
    }
    sub isa { 
        $_[0]->node( 'BoolExpression',
                'Pugs::Runtime::Perl6::Scalar::isa( '. $_[0] . ', ' . $_[1] . ')' );
    }
    sub set {
        $_[0] . ' = ' . $_[1]->scalar;
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
        $_[0]
    }
    sub list { 
        $_[0]->node( 'Seq', [ $_[0] ] );
    }
    sub bind_from {
        $_[0]->name    # not an object
    }
    sub _123__125_ {
        # .{}
        my $self = $_[0];
        my $other = $_[1]->list;
        return '%{' . $_[0] . '}' 
            unless defined $_[1] && $_[1] ne '';
        return $_[0]->node( 'Scalar',  '' . $_[0]->name . '->{' . $_[1] . '}' )
            if $_[1]->list->elems == 1;
        $_[0]->node( 'Array',  '' . $_[0]->name . '->{' . $_[1]->list . '}' )
    }
package Pugs::Emitter::Perl6::Perl5::ValueScalar;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Scalar'; # XXX
    use overload (
        '""'     => sub { 
            '' . $_[0]->name . ''
        },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::ref( '. $_[0] . ')' );
    }
    sub isa { 
        $_[0]->node( 'BoolExpression',
                'Pugs::Runtime::Perl6::Scalar::isa( '. $_[0] . ', ' . $_[1] . ')' );
    }
    sub set {
        $_[0] . ' = ' . $_[1]->scalar;
    }
    sub hash {
        return $_[0]->node( 'HashExpression', $_[0] . '->hash' )
    }
    sub array {
        return $_[0]->node( 'ListExpression', $_[0] . '->array' )
    }
    sub scalar {
        $_[0]
    }
    sub list { 
        $_[0]->node( 'Seq', [ $_[0] ] );
    }
    sub bind_from {
        '\\' . $_[0]->name    # not an object
    }

sub _123__125_ {
    # .{}
    my $self = $_[0];
    my $other = $_[1]->list;
    return '%{' . $_[0] . '}' 
        unless defined $_[1] && $_[1] ne '';
    return $_[0]->node( 'Scalar',  '' . $_[0]->name . '->{' . $_[1] . '}' )
        if $_[1]->list->elems == 1;
    $_[0]->node( 'Array',  '' . $_[0]->name . '->{' . $_[1]->list . '}' )
}
package Pugs::Emitter::Perl6::Perl5::NamedScalar;
    use base 'Pugs::Emitter::Perl6::Perl5::Scalar';
    use overload (
        '""'     => sub { 
            '$' . Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
        },
        fallback => 1,
    );
    sub set {
        #print "SCALAR SET ", Data::Dumper::Dumper( $_[1] );
        #print "SCALAR SET ", $_[1]->scalar;
        $_[0] . ' = ' . $_[1]->scalar
    }
    sub my {
        $Pugs::Emitter::Perl6::Perl5::_V6_PREDECLARE{ $_[0]{name} } = 'my ' . Pugs::Runtime::Common::mangle_var( $_[0]->{name} ) . ' = \( my $' . $_[0]->new_id . ')';
        $_[0]
    }
    sub our {
        $Pugs::Emitter::Perl6::Perl5::_V6_PREDECLARE{ $_[0]{name} } = 'our ' . Pugs::Runtime::Common::mangle_var( $_[0]->{name} ) . ' = \( my $' . $_[0]->new_id . ')';
        $_[0]
    }
    sub bind {
        $_[0]->bind_from . ' = ' . $_[1]->bind_from
    }
    sub bind_from {
        Pugs::Runtime::Common::mangle_var( $_[0]->{name} )
    }

1;

__END__

            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }

        
