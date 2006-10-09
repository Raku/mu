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

1;
