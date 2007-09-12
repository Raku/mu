package GLOBAL;
    use strict;no strict 'refs';

    #require Exporter;
    use Data::Dumper;
    use Carp 'confess';
    #@ISA = qw(Exporter);
    our @EXPORT = qw( 
        print 
        say
        undef
        undefine
        defined
        true
        not
        sleep
        True
        False
        substr
        match_p5rx

        ternary_58__60__63__63__32__33__33__62_
        
        infix_58__60_eq_62_
        infix_58__60_ne_62_
        infix_58__60__61__61__62_
        infix_58__60__33__61__62_
        infix_58__60__38__38__62_
        infix_58__60__124__124__62_
        infix_58__60__126__62_
        infix_58__60__42__62_
        infix_58__60__43__62_
        infix_58__60__45__62_
        infix_58__60__47__62_
        infix_58__60__60__61__62__62_
        infix_58__60__62__62_
        
        prefix_58__60__33__62_
        prefix_58__60__126__62_
        prefix_58__60__64__62_        
        prefix_58__60__43__43__62_
        prefix_58__60__124__62_

    );
    
    # %*ENV
    $GLOBAL::Hash_ENV = bless \%ENV, 'Type_Perl5_Buf_Hash';
    
    ${"GLOBAL::Code_$_"} = \&{"GLOBAL::$_"} for @EXPORT;
    $GLOBAL::Code_import = ::DISPATCH( $::Code, 'new', 
        { code => \&{"GLOBAL::import"}, src => '&GLOBAL::import' } );

    sub init_global {
        #print "Init GLOBAL\n";
        for ( @EXPORT ) {
            #print "Init \$GLOBAL::Code_$_ \n";
            ${"GLOBAL::Code_$_"} = ::DISPATCH( $::Code, 'new', 
                { code => ${"GLOBAL::Code_$_"}, src => '&GLOBAL::'.$_ } ); 
        }
    }

    # XXX - obsolete - GLOBAL is looked up at compile-time
    sub import {
        #print "@_\n";
        my $pkg = _str( $_[0] ); 
        #print "IMPORT $pkg\n";
        ${"${pkg}::Code_$_"} = ::DISPATCH( $::Code, 'new', 
            { code => ${"GLOBAL::Code_$_"}, src => '&GLOBAL::'.$_ } ) 
            for @EXPORT;
    }

    sub _str {
        my $v = $_[0];
        eval { $v = ::DISPATCH( $v, 'str' )->{_value} } if ref($v); 
        print $@ if $@;
        $v;
    }
    sub _int {
        my $v = $_[0];
        eval { $v = ::DISPATCH( $v, 'int' )->{_value} } if ref($v); 
        print $@ if $@;
        $v;
    }

    sub print { 
        print join( '',  map { 
                ::DISPATCH(::DISPATCH($_,'str'),'p5landish');
        } @_ );
    }

    my $undef = ::DISPATCH( $::Undef, 'new', 0 );
    sub undef    { $undef }
    sub undefine { ::DISPATCH( $_[0], 'STORE', $undef ) }
    sub defined  { 
        #print "DEFINED? \n";
        return ::DISPATCH( $::Bit, 'new',0 )
            unless defined $_[0];
        ::DISPATCH( $_[0], 'defined' ) 
    } 
    sub true     { ::DISPATCH( $_[0], 'true' ) }  
    sub not      { ::DISPATCH( $::Bit, 'new', ! ( ::DISPATCH( $_[0], 'true' )->{_value} ) ) }  
    sub True     { ::DISPATCH( $::Bit, 'new',1 ) }  
    sub say      { GLOBAL::print( @_, ::DISPATCH( $::Str, 'new', "\n" ));return True;}
    sub sleep    { CORE::sleep(_int($_[0]));return True;}
    sub False    { ::DISPATCH( $::Bit, 'new',0 ) }  
    sub TODO     {confess("TODO");}

    # TODO - macro
    #  ternary:<?? !!>
    sub ternary_58__60__63__63__32__33__33__62_ : lvalue { 
        #print "ternary: ",caller(2), " $#_ $_[0], $_[1]\n";
        #print ::DISPATCH( $_[0], 'true' );
        ::DISPATCH( $_[0], 'true' )->{_value} ? $_[1] : $_[2] 
    }
    
    # &&
    # TODO - macro
    sub infix_58__60__38__38__62_   { true($_[0])->{_value} ? $_[1] : False }

    # ||
    # TODO - macro
    sub infix_58__60__124__124__62_ { ::DISPATCH( $::Bit, 'new', (::DISPATCH( $_[0], 'true' )->{_value} && $_[0] || ::DISPATCH( $_[1], 'true' )->{_value} && $_[1])) }

    sub infix_58__60_eq_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) eq _str($_[1])) ? 1 : 0) }  # infix:<eq>
    sub infix_58__60_ne_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) ne _str($_[1])) ? 1 : 0) }  # infix:<ne>
    sub infix_58__60__61__61__62_   
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) == _int($_[1])) ? 1 : 0) }  # infix:<==>
    sub infix_58__60__60__61__62__62_
    { ::DISPATCH( $::Int, 'new', (_int($_[0]) <=> _int($_[1]))) }  # infix:<<=>>

    sub infix_58__60__33__61__62_ {  # infix:<!=>
        $::Bit->new(_int($_[0]) != _int($_[1]));
    }

    sub infix_58__60__126__62_      
    { ::DISPATCH( $::Str, 'new', _str( $_[0] ) . _str( $_[1] ) ) }  # infix:<~>
    sub infix_58__60__42__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) * _int( $_[1] ) ) }  # infix:<*>
    sub infix_58__60__43__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) + _int( $_[1] ) ) }  # infix:<+>
    sub infix_58__60__45__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) - _int( $_[1] ) ) }  # infix:<->

    sub infix_58__60__47__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) / _int( $_[1] ) ) }  # infix:</>   XXX - Num
    sub infix_58__60__62__62_  
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) > _int($_[1])) ? 1 : 0) }  # >

    sub substr      
    { 
        #print " substr() parameters: ",
        #    join( ", " ,
        #        _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] ) 
        #    )
        #;
        if ( $#_ == 1 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ) 
                )
            ) 
        }
        if ( $#_ == 2 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ), _int( $_[2] ) 
                )
            ) 
        }
        if ( $#_ == 3 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] ) 
                )
            ) 
        }
        die "Not enough arguments for substr";
    }  

    # prefix:<~>
    sub prefix_58__60__126__62_ { ::DISPATCH( $::Str, 'new', _str( $_[0] ) ) }  

    # prefix:<!>
    sub prefix_58__60__33__62_ { ::DISPATCH( $::Bit, 'new', ! ( ::DISPATCH( $_[0], 'true' )->{_value} ) ) }  

    # prefix:<@>
    sub prefix_58__60__64__62_      { 
        ::DISPATCH( $_[0], 'array' )
    }       

    # prefix:<++>
    sub prefix_58__60__43__43__62_ {
        my $counter = $_[0];
        $counter->{_dispatch_VAR}(
            $counter, 'STORE', ::DISPATCH( $::Int, 'new', _int($counter) + 1)
        );
    }

    # prefix:<|>  explodes a Capture
    sub prefix_58__60__124__62_ {
        my $capture = shift;
        my $array = ::DISPATCH( $capture, 'array' );
        @{ $array->{_value}{_array} };
    }

    sub match_p5rx {
        my ($regex,$string,$pos) = (_str($_[0]),_str($_[1]),_int($_[2]));
        pos($string) = $pos;
        my $bool = $string =~ /\G$regex/gc;
        print "regex:<$regex> string:<$string>\n";
        if ($bool) {
            print "matched up to:",pos($string),"\n";
            ::DISPATCH($::Match,'new',{
                    match_str=>$_[1],
                    from=>$_[2],
                    to=>::DISPATCH($::Int,'new',pos($string)),
                    bool=>True
            });
        } else {
            print "false match\n";
            ::DISPATCH($::Match,'new',{bool=>False});
        }
    }



{
    GLOBAL::init_global;
}

1;
