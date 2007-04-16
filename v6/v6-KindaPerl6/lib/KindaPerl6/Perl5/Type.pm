# a "Type" is a class
# it defines:
# - the storage (array, scalar, hash), also: list
# - read-only-ness
# - type-checks
# - interpolation ( @a vs $a=list )

# - Arrays could use hash storage by default, such that $a[100000] is not a problem
# - but then 'for' and 'map' would always need to sort the values

# TODO
# - ITERATOR()

use Carp;

# virtual classes
package Int;
package Buf;
package Scalar;
package Array;
package Hash;
package Undef;

package Type_Constant;
    # $x = bless \( do{ my $v = 42 } ), 'Type_Constant';
    sub IS_ARRAY { 0 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        return $_[0]
            if $_[1] == 0;
        return $GLOBAL::undef;
    }
    sub LOOKUP {
        warn "not a hash";
    }
    sub STORE  { 
        warn "Can't modify constant item";
        $_[0];
    }
    sub FETCH  { 
        $_[0];
    }
package Type_Constant_Undef;
    our @ISA = ( 'Type_Constant', 'Undef' );
    sub perl { 'undef' }
package Type_Constant_Int;
    our @ISA = ( 'Type_Constant', 'Int' );
    sub perl { ${$_[0]} }
package Type_Constant_Num;
    our @ISA = ( 'Type_Constant', 'Num' );
    sub perl { ${$_[0]} }
package Type_Constant_Bit;
    our @ISA = ( 'Type_Constant', 'Bit' );
    sub perl { ${$_[0]} ? 1 : 0 }
package Type_Constant_Buf;
    # $x = bless \( do{ my $v = 'abc' } ), 'Type_Constant_Buf';
    our @ISA = ( 'Type_Constant', 'Buf' );
    our $AUTOLOAD;
    sub perl { "'${$_[0]}'" }
    sub DESTROY { }
    sub AUTOLOAD {
        # allow: 'Dog'.new
        #require Data::Dump::Streamer;
        #print "Buf AUTOLOAD: $AUTOLOAD - ", Data::Dump::Streamer::Dump( \@_ );
        my $self = shift;
        my $meth = $AUTOLOAD;
        $meth =~ s/.*:://;   # strip fully-qualified portion
        #print "self $$self, AUTOLOAD: $meth \n";
        $$self->$meth( @_ );
    }    
    
package Type_Scalar;
    our @ISA = 'Scalar';
    our $AUTOLOAD;
    sub perl { $_[0]->FETCH->perl }
    sub IS_ARRAY { 0 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        # $scalar->INDEX( 0 ) just works
        # $scalar->INDEX( 2 ) returns undef
        # $scalar->INDEX( 2 )->STORE(...) autovivifies an Array if the scalar is undef
        #print "INDEX: ",Data::Dump::Streamer::Dump( \@_ );
        return $_[0][0]->INDEX( $_[1] )
            if $_[0][0]->IS_ARRAY;
        return $_[0]
            if $_[1]->FETCH == 0;
        #print "Return undef on unexisting Array\n";
        return bless [ $_[0], $_[1] ], 'Type_Proxy_Array_Scalar';
    }
    sub LOOKUP {
        return $_[0][0]->LOOKUP( $_[1] )
            if $_[0][0]->IS_HASH;
        warn "not a hash"
            if defined ${$_[0]->FETCH};
        $_[0]->STORE( bless { }, 'Type_Hash' );
        return $_[0][0]->LOOKUP( $_[1] )
    }
    sub STORE  { 
        $_[0][0] = $_[1]->FETCH;
        $_[0][1]{ $_[0][2] }++;
        $_[0];
    }
    sub BIND   {
        # XXX - how about %_MODIFIED ?
        $_[0] = $_[1];
    }
    sub FETCH  { 
        $_[0][0];
    }
    sub DESTROY { }
    sub AUTOLOAD {
        # allow: $x.new
        # - $x.FETCH.new is better - disable AUTOLOAD ???
        #require Data::Dump::Streamer;
        #print "Scalar AUTOLOAD: $AUTOLOAD - ", Data::Dump::Streamer::Dump( \@_ );
        my $self = shift;
        my $meth = $AUTOLOAD;
        $meth =~ s/.*:://;   # strip fully-qualified portion
        #print "self $$self, AUTOLOAD: $meth \n";
        $$self->$meth( @_ );
    }    

package Type_Proxy_Array_Scalar;
    # $x = bless \( do{ my $v = 42 } ), 'TypeConstant';
    our @ISA = 'Type_Constant_Undef';
    sub IS_ARRAY { 0 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        return $_[0][0]->INDEX( $_[0][1] )->INDEX( $_[1] )
            if exists $_[0][2];
        return $GLOBAL::undef;
    }
    sub LOOKUP {
        return $_[0][0]->INDEX( $_[0][1] )->LOOKUP( $_[1] )
            if exists $_[0][2];
        warn "not a hash";
    }
    sub STORE  { 
        #print "PROXY: ",Data::Dump::Streamer::Dump( ${$_[0][0]->FETCH} );
        warn "Can't modify constant item"
            if defined ${$_[0][0]->FETCH};
        $_[0][0]->STORE( bless [ ], 'Type_Array' );
        $_[0][2] = $_[0][0]->INDEX( $_[0][1] )->STORE( $_[1] );
        # $_[0];
    }
    sub FETCH  { 
        return $_[0][0]->INDEX( $_[0][1] )
            if exists $_[0][2];
        return $GLOBAL::undef;
    }
package Type_Array;
    # $x = bless [ ], 'TypeIntArray';
    our @ISA = 'Array';
    sub IS_ARRAY { 1 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        # self, index
        # autovivify
            $_[0][ ${$_[1]->FETCH} ]
        or  $_[0][ ${$_[1]->FETCH} ] = bless \( do{ my $v } ), 'Type_Scalar' 
    }
    sub LOOKUP {
        warn "not a hash";
    }
    sub STORE  { 
        if ( $_[1]->IS_ARRAY ) {
            @{$_[0]} = @{$_[1]};
        }
        elsif ( ${$_[1]}->IS_ARRAY ) {
            @{$_[0]} = @{${$_[1]}}
        }
        else {
            @{$_[0]} = ${$_[1]}
        }
        # @{$_[0]} 
    }
    sub FETCH  { 
        $_[0];
    }
package Type_List;
    # $x = bless [ ], 'TypeIntArray';
    our @ISA = 'List';
    sub IS_ARRAY { 1 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        # self, index
        # autovivify
            $_[0][ ${$_[1]->FETCH} ]
        or  $_[0][ ${$_[1]->FETCH} ] = bless \( do{ my $v } ), 'Type_Scalar' 
    }
    sub LOOKUP {
        warn "not a hash";
    }
    sub STORE  { 
        warn "Can't modify constant item";
        $_[0] 
    }
    sub FETCH  { 
        $_[0];
    }
package Type_Hash;
    # $x = bless { }, 'Type_Hash';
    our @ISA = 'Hash';
    sub IS_ARRAY { 0 }
    sub IS_HASH  { 1 }
    sub INDEX  { 
        return $GLOBAL::undef;
    }
    sub LOOKUP {
        # self, index
        # autovivify
            $_[0]{ ${$_[1]->FETCH} }
        or  $_[0]{ ${$_[1]->FETCH} } = bless \( do{ my $v } ), 'Type_Scalar' 
    }
    sub STORE  { 
        if ( $_[1]->IS_HASH ) {
            %{$_[0]} = %{$_[1]};
        }
        elsif ( ${$_[1]}->IS_HASH ) {
            %{$_[0]} = %{${$_[1]}}
        }
        else {
            # XXX - pairs
            warn "Hash.STORE not finished";
            %{$_[0]} = ${$_[1]}
        }
        # @{$_[0]} 
    }
    sub FETCH  { 
        $_[0];
    }
package Type_Perl5_Buf_Hash;
    # $x = bless { }, 'Type_Hash';
    # This is used to implement %*ENV
    our @ISA = 'Type_Hash';
    sub LOOKUP {
        # self, index
        bless \( $_[0]{ ${$_[1]->FETCH} } ), 'Type_Constant_Buf' 
    }
    
1;
__END__
{
    my ($x,$y);
    
    $x = bless \( do{ my $v } ), 'TypeInt';
    
    $y = $x; 
    $x->STORE( 3 ); 
    print $$y, " typed x (3)\n"; 
    print $$x, "\n";
    $x->STORE( 'a' );
    
}

{
    my ($x,$y);

    $x = bless [ ], 'TypeIntArray';

    $y = $x; 
    $x->INDEX( 0 )->STORE( 3 ); 
    
    print ${$x->INDEX( 0 )}, " typed y (1)\n"; 
    $y->INDEX( 0 )->STORE( 4 ); 
    print ${$x->INDEX( 0 )}, "\n";
}
