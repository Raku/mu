package Pugs::Emitter::Perl6::Perl5::Hash;
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
        $_[0]->node( 'str', 'Hash' );
    }
    sub isa { 
        my $self = $_[0];
        return $_[0]->WHAT->eq( $_[1]->WHAT ); 
        #return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
    }
    sub get {
        my $self = $_[0];
        return $self->name;
    }
    sub set {
        my $self = $_[0];
        return $self->name . ' = ' . $_[1]->hash->get;
    }
    sub str {
        return $_[0]->node( 'StrExpression', ' Pugs::Runtime::Perl6::Hash::str( \\' . $_[0] . ' ) ' )
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
    sub elems {
        return $_[0]->node( 'IntExpression',  '(scalar keys ' . $_[0]->name . ')' )
    }
    sub hash {
        $_[0]
    }
    sub array {
        return $_[0]->node( 'ListExpression',  
            '@{[' . $_[0]->name . ']}' )   
    }    
    sub scalar {
        return $_[0]->node( 'Scalar', '( bless \\' . $_[0] . ", 'Pugs::Runtime::Perl6::Hash' )" )
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
package Pugs::Emitter::Perl6::Perl5::SeqHash;
    use base 'Pugs::Emitter::Perl6::Perl5::Hash';
    use overload (
        '""'     => sub { 
            '{' . join( ', ', map { $_->boxed } @{$_[0]->{name}} ) . '}' 
        },
        fallback => 1,
    );
    sub new {
        # - interpolate Pair Values at compile-time
        # - variables starting with '$' never interpolate
        #   even if they contain a Pair or a Hash
        # - variables starting with '%' or '@' do interpolate
        #   at runtime.
        # - hash and list expressions interpolate
        #   such as %$h
        # - scalar expressions don't interpolate
        #   such as $%h
        # - values starting with '&' don't interpolate
        # - expressions are called in list context

        #print "SeqHash.new ",Data::Dumper::Dumper(\@_);
        my @self = map {
            #print "SeqHash.elem ",$_->WHAT->{name}," ",Data::Dumper::Dumper($_);
            $_->WHAT->{name} eq 'Pair' 
            ? ( $_->{name}[0], $_->{name}[1] )
            : $_
            } @{$_[1]->{name}};  
        return bless { name => \@self }, $_[0];
    }
    sub scalar {
        return $_[0]->node( 'Scalar', '( bless ' . $_[0] . ", 'Pugs::Runtime::Perl6::Hash' )" )
    }
    sub str {
        return $_[0]->node( 'StrExpression', ' Pugs::Runtime::Perl6::Hash::str( ' . $_[0] . ' ) ' )
    }

package Pugs::Emitter::Perl6::Perl5::NamedHash;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value'; # XXX
    
    use overload (
        '""'     => sub { 
            '%' . $_[0]->name
        },
        fallback => 1,
    );
    sub name {
        my $name = Pugs::Runtime::Common::mangle_var( $_[0]->{name} );
        $name =~ s/\%/HASH/;
        '$' . $name
    }
    sub bind {
        $_[0]->bind_from . ' = ' . $_[1]->bind_from
    }
    sub bind_from {
        $_[0]->name
    }
    sub WHAT { 
        $_[0]->node( 'str', 'Hash' );
    }

sub isa { 
    my $self = $_[0];
    return $_[0]->WHAT->eq( $_[1]->WHAT ); 
}

sub get {
    my $self = $_[0];
    $self->name;
}

sub set {
    my $self = $_[0];
    print "NamedHash set ", Dumper( $_[1] );
    $self->name . ' = ' . $_[1]->hash->get;
}

    sub str {
        $_[0]->node( 'StrExpression', ' Pugs::Runtime::Perl6::Hash::str( \\' . $_[0] . ' ) ' )
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
    $_[0]->node( 'ListExpression', '(keys ' . $_[0]->name . ')' )   
}

sub num {
    $_[0]->elems
} 

sub int {
    $_[0]->elems
} 

sub true { 
    $_[0]->node( 'BoolExpression', $_[0]->elems )
}

sub elems {
    $_[0]->node( 'IntExpression',  '(scalar keys ' . $_[0]->name . ')' )
}


sub hash {
    $_[0]
}

sub array {
    #print "\@Hash->Array\n";
    $_[0]->node( 'ListExpression', '@{[' . $_[0]->name . ']}' )   
}

sub scalar {
    $_[0]->node( 'Hash', '( bless \\' . $_[0] . ", 'Pugs::Runtime::Perl6::Hash' )" )
}
    sub my {
        $Pugs::Emitter::Perl6::Perl5::_V6_PREDECLARE{ $_[0]{name} } = 'my ' . $_[0]->name;
        $_[0]
    }
sub _123__125_ {
    # .{}
    print Data::Dumper::Dumper( @_ );
    return $_[0] unless defined $_[1] && $_[1] ne ''; 
    $_[0]->node( 'Scalar',  '${' . $_[0]->name . '->{' . $_[1]->list . '}}' )
}

1;

__END__

            if ($n->{method}{dot_bareword} eq 'isa') {
                return 'Pugs::Runtime::Perl6::Scalar::isa( \\'. _emit( $n->{self} ) . ', ' . _emit( $n->{param} ) . ')';
            }

        

1;
