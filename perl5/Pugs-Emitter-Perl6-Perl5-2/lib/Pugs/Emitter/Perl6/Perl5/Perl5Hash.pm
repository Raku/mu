package Pugs::Emitter::Perl6::Perl5::Perl5Hash;
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
        $_[0]->node( 'str', 'Hash' );
    }

sub _dollar_name {
    my $name = $_[0]->{name};
    $name =~ s/\%/\$/;
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
    print "perl5hash set ", Dumper( $_[1] );
    return $self->name . ' = ' . $_[1]->hash->get;
}

    sub str {
        return $_[0]->node( 'StrExpression', ' Pugs::Runtime::Perl6::Hash::str( \\' . $_[0] . ' ) ' )
    }
    sub perl {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::perl( \\'. $_[0] . ')' );
    }
    sub yaml {
        $_[0]->node( 'StrExpression',
                'Pugs::Runtime::Perl6::Scalar::yaml( \\'. $_[0] . ')' );
    }
    
sub defined {
    # TODO
}

sub kv {
    $_[0]->array
}

sub keys {
    return $_[0]->node( 'ListExpression', '(keys ' . $_[0]->name . ')' )   
}

sub num {
    return $_[0]->elems
} 

sub int {
    return $_[0]->elems
} 

sub true { 
    return $_[0]->node( 'BoolExpression', $_[0]->elems )
}

sub elems {
    return $_[0]->node( 'IntExpression',  '(scalar keys ' . $_[0]->name . ')' )
}


sub hash {
    $_[0]
}

sub array {
    #print "\@Hash->Array\n";
    return $_[0]->node( 'ListExpression', '@{[' . $_[0]->name . ']}' )   
}

sub scalar {
    return $_[0]->node( 'Hash', '( bless \\' . $_[0] . ", 'Pugs::Runtime::Perl6::Hash' )" )
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

        
