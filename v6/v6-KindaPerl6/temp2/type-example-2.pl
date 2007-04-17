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

package TypeConstant;
    # $x = bless \( do{ my $v = 42 } ), 'TypeConstant';
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
$GLOBAL::undef = bless \( do{ my $v = undef } ), 'TypeConstant';
package TypeInt;
    # $x = bless \( do{ my $v } ), 'TypeInt';
    our @ISA = 'Scalar';
    sub IS_ARRAY { 0 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        # $scalar->INDEX( 0 ) just works
        return ${$_[0]}->INDEX( $_[1] )
            if ${$_[0]}->IS_ARRAY;
        return $_[0]
            if $_[1] == 0;
        return $GLOBAL::undef;
    }
    sub LOOKUP {
        return ${$_[0]}->LOOKUP( $_[1] )
            if ${$_[0]}->IS_HASH;
        warn "not a hash";
    }
    sub STORE  { 
        # <[particle]> yep, check if it can ->int, and call it if so, otherwise die or something
        warn "not a number: $_[1]" if $_[1] ne $_[1]+0;
        ${$_[0]} = $_[1];
        $_[0];
    }
package TypeIntArray;
    # $x = bless [ ], 'TypeIntArray';
    our @ISA = 'Array';
    sub IS_ARRAY { 1 }
    sub IS_HASH  { 0 }
    sub INDEX  { 
        # self, index
        # autovivify
            $_[0][$_[1]]
        or  $_[0][$_[1]] = bless \( do{ my $v } ), 'TypeInt' 
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
    # Autovivification
    # problem - INDEX() thinks that $a[1] := undef or $a[1] := 0 are uninitialized elements
    # solution - make := to non-blessed read-only, until another := happens
    # but - make_ro can't be undone ???
    # solution - make 'undef' a ro-singleton, in which 'STORE' dies
    # but - *all* scalars must have boxed values
    # solution - test with exists()
    # but - *all* access must be made with INDEX(), in order to avoid p5 autovivification
    # but - INDEX( 10000 ) will not DWIM
    # but - this is not better than the previous solution, because
    #   binding only works if the values are objects
    #   $a = undef;
    #   $b = $a;   # ok
    #   $a->STORE( 3 );  # error - ok
    #   $$a;        # error - not a reference (fixable with $a = \undef; )
    #   $$a->meth;  # error - no autoboxing

    # Inlining
    # it is possible to combine INDEX+FETCH and INDEX+STORE

package main;
use Data::Dump::Streamer;

# TODO - Array, Hash (autovivification) - use an error handler?
# TODO - Scalar holding Array, Scalar holding Hash

# Roles
# but true, but false
# $obj->{Role_bool} = sub { 1 }
# if ( ( $obj->{Role_bool} || $obj->HOW->get_method('bool') )->() ) { ... }

# Traits
# is rw, is ro

# Signature-specific
# is copy

# ??? is context

use strict;

{
    my ($x,$y);
    
    $x = bless \( do{ my $v } ), 'TypeInt';
    
    $y = $x; 
    $x->STORE( 3 ); 
    #print "x is a ",ref($x),"\n", Dump($x);
    #print "y is a ",ref($y),"\n", Dump($y);
    
    print $$y, " typed y (1)\n"; 
    $y->STORE( 4 ); 
    print $$x, "\n";
    
    $y->STORE( 42 );
    print $$y, " typed y (2)\n"; 
    print $$x, "\n";
    $y->STORE( 'a' );
    
    $x->STORE( 99 );
    print $$y, " typed x (3)\n"; 
    print $$x, "\n";
    $x->STORE( 'a' );
    
}

{
    my ($x,$y);

    $x = bless [ ], 'TypeIntArray';

    $y = $x; 
    $x->INDEX( 0 )->STORE( 3 ); 
    #print "x is a ",ref($x),"\n", Dump($x);
    #print "y is a ",ref($y),"\n", Dump($y);
    
    print ${$x->INDEX( 0 )}, " typed y (1)\n"; 
    $y->INDEX( 0 )->STORE( 4 ); 
    print ${$x->INDEX( 0 )}, "\n";
    
    $y->INDEX( 0 )->STORE( 42 );
    print ${$y->INDEX( 0 )}, " typed y (2)\n"; 
    print ${$x->INDEX( 0 )}, "\n";
    $y->INDEX( 0 )->STORE( 'a' );
    
    $x->INDEX( 0 )->STORE( 99 );
    print ${$y->INDEX( 0 )}, " typed x (3)\n"; 
    print ${$x->INDEX( 0 )}, "\n";
    $x->INDEX( 0 )->STORE( 'a' );

}