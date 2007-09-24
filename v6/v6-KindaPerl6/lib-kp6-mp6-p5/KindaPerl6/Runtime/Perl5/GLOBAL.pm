package GLOBAL;
    use strict;no strict 'refs';

    #require Exporter;
    use Data::Dumper;
    use Carp 'confess';
    #@ISA = qw(Exporter);
    our @EXPORT = qw( 
        VAR
        print 
        say
        warn
        die
        exit
        undef
        undefine
        defined
        exists
        true
        not
        sleep
        True
        False
        substr
        length
        match_p5rx
        require
        slurp
        keys
        push
        qw

        print_backtrace

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
        infix_58__60__62__61__62_
        infix_58__60__62__62_
        infix_58__60__60__62_
        
        prefix_58__60__33__62_
        prefix_58__60__36__62_
        prefix_58__60__37__62_
        prefix_58__60__126__62_
        prefix_58__60__64__62_        
        prefix_58__60__43__43__62_
        prefix_58__60__124__62_
        prefix_58__60__45__62_

    );
    
    # %*ENV
    $GLOBAL::Hash_ENV = 
        ::DISPATCH( $::Hash, 'new', {
            _hash => {
                map { (
                        $_,
                        ::DISPATCH( $::Str, 'new', $ENV{$_} )
                    ) } keys %ENV
            }
        } );

=pod 
    # XXX - hash key autovivification is not rw! please fix in MOP.pm
       
    # %*ENV
    $GLOBAL::Hash_ENV = 
        ::DISPATCH( $::Hash, 'new', {
                _hash => { } 
            } ); 
    for ( keys %ENV ) {
        ::DISPATCH( 
            ::DISPATCH( $GLOBAL::Hash_ENV, 'LOOKUP', ::DISPATCH( $::Str, 'new', $_ ) ),
            'STORE',
            ::DISPATCH( $::Str, 'new', $ENV{$_} )
        );
    }    
=cut
    
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
        $v = ::DISPATCH( $v, 'str' )   if ref($v); 
        $v = ::DISPATCH( $v, 'FETCH' ) if ref($v);  # .str may return a Scalar
        return $v->{_value}            if ref($v); 
        $v;
    }
    sub _int {
        my $v = $_[0];
        $v = ::DISPATCH( $v, 'int' )   if ref($v); 
        $v = ::DISPATCH( $v, 'FETCH' ) if ref($v);  # .int may return a Scalar
        return $v->{_value}            if ref($v); 
        $v;
    }

    sub VAR {
        # returns a proxy object that dispatches to the container
        my $container = shift;
        return $container 
            unless exists $container->{_dispatch_VAR};
        return {
            _dispatch => sub {
                    #print "dispatching method $_[1]\n";
                    shift;
                    $container->{_dispatch_VAR}->( $container, @_ );
                }
        };
    }

    sub print { 
        CORE::print join( '',  map { 
                _str( $_ )
        } @_ );
    }

      sub keys { ::DISPATCH( $_[0], 'keys' ) }
      sub GLOBAL::push { ::DISPATCH( $_[0], 'push' ) }

    my $undef = ::DISPATCH( $::Undef, 'new', 0 );
    sub undef    { $undef }
    sub undefine { ::DISPATCH( $_[0], 'STORE', $undef ) }
    sub defined  { 
        #print "DEFINED? \n";
        return ::DISPATCH( $::Bit, 'new',0 )
            unless defined $_[0];
        ::DISPATCH( $_[0], 'defined' ) 
    } 
    sub exists  { 
        ::DISPATCH( VAR($_[0]), 'exists' );
    } 
    sub true     { ::DISPATCH( $_[0], 'true' ) }  
    sub not      { ::DISPATCH( $::Bit, 'new', ! ( ::DISPATCH( $_[0], 'true' )->{_value} ) ) }  
    sub True     { ::DISPATCH( $::Bit, 'new',1 ) }  
    sub say      { GLOBAL::print( @_, ::DISPATCH( $::Str, 'new', "\n" ));return True;}
    sub warn     { CORE::warn( join '', map { _str($_) } @_ )}
    sub die      { confess( join '', map { _str($_) } @_ )}
    sub exit     { CORE::exit() }
    sub sleep    { CORE::sleep(_int($_[0]));return True;}
    sub False    { ::DISPATCH( $::Bit, 'new',0 ) }  
    sub TODO     {confess("TODO");}
    sub qw {
        ::DISPATCH( $::Array, 'new', { 
                _array => [ 
                    map {
                            ::DISPATCH( $::Str, 'new', $_ )
                        }
                        eval("qw/"._str($_[0])."/") 
                ] 
            } )
    }

    # TODO - macro
    #  ternary:<?? !!>
    sub ternary_58__60__63__63__32__33__33__62_ : lvalue { 
        #print "ternary: ",caller(2), " $#_ $_[0], $_[1]\n";
        #print ::DISPATCH( $_[0], 'true' );
        ::DISPATCH( $_[0], 'true' )->{_value} ? $_[1] : $_[2] 
    }
    
    # &&
    # TODO - macro (moved to EmitPerl5 for now)
    # sub infix_58__60__38__38__62_   { true($_[0])->{_value} ? $_[1] : False }

    # ||
    # TODO - macro (moved to EmitPerl5 for now)
    # sub infix_58__60__124__124__62_ { ::DISPATCH( $::Bit, 'new', (::DISPATCH( $_[0], 'true' )->{_value} && $_[0] || ::DISPATCH( $_[1], 'true' )->{_value} && $_[1])) }

    sub infix_58__60_eq_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) eq _str($_[1])) ? 1 : 0) }  # infix:<eq>
    sub infix_58__60_ne_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) ne _str($_[1])) ? 1 : 0) }  # infix:<ne>
    sub infix_58__60__61__61__62_   
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) == _int($_[1])) ? 1 : 0) }  # infix:<==>
    sub infix_58__60__60__61__62__62_
    { ::DISPATCH( $::Int, 'new', (_int($_[0]) <=> _int($_[1]))) }  # infix:<<=>>
    sub infix_58__60__62__61__62_
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) >= _int($_[1]))) } # infix: <>=>
    sub infix_58__60__60__62_
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) < _int($_[1]))) } # infix: <<>
    sub infix_58__60__33__61__62_ {
        ::DISPATCH( $::Bit, 'new', (_int($_[0]) != _int($_[1]))); }  # infix:<!=>

    sub GLOBAL::require {
        eval "require "._str($_[0]);
        die $@ if $@;
    }

    sub slurp {
        warn '#XXX# Slurp only a prototype here!';
        return ::DISPATCH($::Str, 'new', join '', <>);
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
            #warn "STACK TRACE--------------------------------------\n";
            #warn join("\n", map { join ', ', caller($_) } 1..6)."\n";
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

    sub length {
        ::DISPATCH( $::Int, 'new', length(_str( $_[0] )) );
    }


    # prefix:<$> and prefix:<%>
    # XXX: TODO: This should force scalar|hash context.
    sub prefix_58__60__36__62_ { ::DISPATCH( $_[0], 'scalar') }
    sub prefix_58__60__37__62_ { ::DISPATCH( $_[0], 'hash') }

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

    # -1
    sub prefix_58__60__45__62_ {
        ::DISPATCH( $::Int, 'new', - _int($_[0]) )
    }

    sub match_p5rx {
        my ($regex,$string,$pos) = (_str($_[0]),_str($_[1]),_int($_[2]));
        if ($_[1] == $::Undef) {
            $string = _str($_);
        }
        pos($string) = $pos;
        if ($ENV{KP6_TOKEN_DEBUGGER}) {
            print ">>> inside p5 token $regex at $pos of ($string)\n";
        }
        my $bool = $string =~ /\G$regex/gc;
        #print "regex:<$regex> string:<$string>\n";
        if ($bool) {
            if ($ENV{KP6_TOKEN_DEBUGGER}) {
                print "<<< p5 token $regex returned true\n";
            }
            #print "matched up to:",pos($string),"\n";
            my $m = ::DISPATCH($::Match,'new',
                               ::DISPATCH(
                                          $::NamedArgument,
                                          "new",
                                          {
                                           _argument_name_ => ::DISPATCH( $::Str, 'new', 'match_str' ),
                                            value => $_[1]
                                          }
                                         ),
                               ::DISPATCH(
                                          $::NamedArgument,
                                          "new",
                                          {
                                           _argument_name_ => ::DISPATCH( $::Str, 'new', 'from' ),
                                            value => $_[2]
                                          }
                                         ),
                               ::DISPATCH(
                                          $::NamedArgument,
                                          "new",
                                          {
                                           _argument_name_ => ::DISPATCH( $::Str, 'new', 'to' ),
                                            value => ::DISPATCH($::Int,'new',pos($string)),
                                          }
                                         ),
                               ::DISPATCH(
                                          $::NamedArgument,
                                          "new",
                                          {
                                           _argument_name_ => ::DISPATCH( $::Str, 'new', 'bool' ),
                                            value => ::DISPATCH($::Bit,'new',1)
                                          }
                                         ),
                           );
            return $m;
        } else {
            if ($ENV{KP6_TOKEN_DEBUGGER}) {
                print "<<< p5 token $regex returned false\n";
            }
            #print "false match\n";
            my $m = ::DISPATCH($::Match,'new',
                               ::DISPATCH(
                                          $::NamedArgument,
                                          "new",
                                          {
                                           _argument_name_ => ::DISPATCH( $::Str, 'new', 'bool' ),
                                           value => ::DISPATCH($::Bit,'new',0)
                                          }
                                         ),
                              );
            return $m;
        }
    }

    sub print_backtrace {
        package DB;
        my $depth = 0;
        while (my ($package, $filename, $line, $subroutine, $hasargs,$wantarray, $evaltext, $is_require, $hints, $bitmask) = caller(++$depth)) {
            if ($subroutine ne '(eval)') {
                print "$subroutine(",join(',',map {
                        package GLOBAL;
                        if (ref $_) {
                            _str($_);
                        } else {
                            $_;
                        }
                } @DB::args),")\n";
            }
        }
    }


{
    GLOBAL::init_global;
}

1;
