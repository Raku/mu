package Pugs::Emitter::Perl6::Perl5::Array;

    # TODO after __END__

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
        $_[0]->node( 'str', 'Array' );
    }

sub isa { 
    return $_[0]->WHAT->eq( $_[1] ); 
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
    return $_[0]->node( 'StrExpression', '"' . $_[0] . '"' )
}

    sub perl {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::perl( '. $_[0] . ')' );
    }
    sub yaml {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::yaml( '. $_[0] . ')' );
    }
    
sub kv {
    my $tmp = "( map { ( \$_, ".$_[0]->name."[\$_] ) } 0..".$_[0]->name."-1 )"; 
    return $_[0]->node( 'ListExpression',  $tmp );
}

sub keys {
    my $tmp = "( 0..".$_[0]->name."-1 )"; 
    return $_[0]->node( 'ListExpression',  $tmp );
}

sub values {
    return $_[0]
} 

sub num {
    return $_[0]->elems
} 

sub int {
    return $_[0]->elems
} 

sub elems {
    return $_[0]->node( 'IntExpression',  'scalar ' . $_[0]->name )
}

sub defined {
    return $_[0]->node( 'BoolExpression',  
        '(defined ' . $_[0]->_dollar_name . '[' . $_[1] . '])' )
}

sub exists {
    return $_[0]->node( 'BoolExpression',  
        '(exists ' . $_[0]->_dollar_name . '[' . $_[1] . '])' )
}

sub delete {
    die "TODO";
    'delete ' . $_[0]->_dollar_name . '[' . $_[1] . ']';
}

sub hash {
    return $_[0]->node( 'HashExpression', '%{{' . $_[0]->name . '}}' )
}

sub array {
    $_[0];
}

sub scalar {
    $_[0];
    # return $_[0]->node( 'Array', 'bless \\' . $_[0]->name . ", 'Pugs::Runtime::Perl6::Array'" )
}
    sub list { 
        #print "Array->List ", $_[0]->name , "\n";
        $_[0]->node( 'ListExpression', $_[0]->name );
    }

sub _91__93_ {
    # .[]
    my $self = $_[0];
    my $other = $_[1]->list;
    return $_[0] unless $other;  # TODO
    return $self->_dollar_name . '[' . $other . ']';
}
package Pugs::Emitter::Perl6::Perl5::SeqArray;
    use base 'Pugs::Emitter::Perl6::Perl5::Array';
    use overload (
        '""'     => sub { 
            '(' . join( ', ', map {
                #print "Seq.elem ",$_," ",Data::Dumper::Dumper($_);                 
                $_->bind_from 
            } @{$_[0]->{name}} ) . ')' 
        },
        fallback => 1,
    );
    sub scalar {
        return $_[0]->node( 'Scalar', '( bless ' . $_[0] . ", 'Pugs::Runtime::Perl6::Array' )" )
    }
    sub str {
        return $_[0]->node( 'StrExpression', ' "@[{ ' . $_[0] . ' ]}" ' )
    }
    sub list {
        return $_[0]->node( 'Seq', $_[0]{name} )
    }

package Pugs::Emitter::Perl6::Perl5::NamedArray;

    # TODO after __END__

    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value'; # XXX
    use overload (
        '""'     => sub { 
            '@' . $_[0]->name
        },
        fallback => 1,
    );
    sub name {
        my $name = Pugs::Runtime::Common::mangle_var( $_[0]->{name} );
        $name =~ s/\@/ARRAY/;
        '$' . $name
    }
    sub bind {
        $_[0]->bind_from . ' = ' . $_[1]->bind_from
    }
    sub bind_from {
        $_[0]->name
    }
    sub WHAT { 
        $_[0]->node( 'str', 'Array' );
    }

sub isa { 
    $_[0]->WHAT->eq( $_[1] ); 
}

sub get {
    my $self = $_[0];
    $self->name;
}

sub set {
    my $self = $_[0];
    # XXX box
    '( ' . $self->name . ' = [' . $_[1]->array . '] )';
}

    sub str {
        $_[0]->node( 'StrExpression', '"' . $_[0] . '"' )
    }

    sub perl {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::perl( \\'. $_[0] . ')' );
    }
    sub yaml {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::yaml( \\'. $_[0] . ')' );
    }
    
sub kv {
    my $tmp = "( map { ( \$_, ".$_[0]->name."[\$_] ) } 0..".$_[0]->name."-1 )"; 
    $_[0]->node( 'ListExpression',  $tmp );
}

sub keys {
    my $tmp = "( 0..".$_[0]->name."-1 )"; 
    $_[0]->node( 'ListExpression',  $tmp );
}

    sub values {
        $_[0]
    } 
    sub list { 
        $_[0]
    }

sub num {
    $_[0]->elems
} 

sub int {
    $_[0]->elems
} 

sub elems {
    $_[0]->node( 'IntExpression',  'scalar @{' . $_[0]->name . '}' )
}

sub true { 
    $_[0]->node( 'BoolExpression', $_[0]->elems )
}

sub defined {
    $_[0]->node( 'BoolExpression',  '1' )
}

sub exists {
    $_[0]->node( 'BoolExpression',  
        '(exists ' . $_[0]->_dollar_name . '[' . $_[1] . '])' )
}

sub delete {
    die "TODO";
    'delete ' . $_[0]->_dollar_name . '[' . $_[1] . ']';
}

sub hash {
    $_[0]->node( 'HashExpression', '%{{' . $_[0] . '}}' )
}

sub array {
    $_[0];
}

sub scalar {
    $_[0]->node( 'Array', '( bless \\' . $_[0] . ", 'Pugs::Runtime::Perl5Container::Array' )" )
}

sub shift {
    $_[0]->node( 'Scalar', 'shift ' . $_[0] )
}

    sub my {
        $Pugs::Emitter::Perl6::Perl5::_V6_PREDECLARE{ $_[0]{name} } = 'my ' . $_[0]->name;
        $_[0]
    }

sub _91__93_ {
    # .[]
    return $_[0] unless defined $_[1]; 
    #print "index: $_[1] ", $_[1]->list->elems, Data::Dumper::Dumper( $_[1] );
    #print '${' . $_[0]->name . '->[' . $_[1] . ']}',"\n";
    return $_[0]->node( 'Scalar',  '' . $_[0]->name . '->[' . $_[1] . ']' )
        if $_[1]->list->elems == 1;
    $_[0]->node( 'Scalar',  '' . $_[0]->name . '->[' . $_[1]->list . ']' )
}

1;

__END__


            if ($n->{method}{dot_bareword} eq 'map') {
                my $param = $n->{param}{fixity} eq 'circumfix' ? $n->{param}{exp1} : undef;
                my $code = $param->{bare_block} ? 'sub { '._emit($param).' }' : _emit($param);
                return 'Pugs::Runtime::Perl6::Array::map([\('.$code.', '. _emit( $n->{self} ).')], {})';
            }
            if (  $n->{method}{dot_bareword} eq 'delete' 
               || $n->{method}{dot_bareword} eq 'exists' 
               ) {
                my $self = _emit($n->{self});
                $self =~ s{\@}{\$};
                return _emit( $n->{method} ).' '.$self.'['._emit($n->{param}).']';
            }


            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }

