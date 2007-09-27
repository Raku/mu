
use v6-alpha;

grammar KindaPerl6::Grammar {

token term {
    | <var>     { return $$<var> }     # $variable
    | <arrow_sub> { return $$<arrow_sub> }     # -> $param { code... }
    | <prefix_op> <exp> 
          { return ::Apply(
            'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<' ~ $<prefix_op> ~ '>', namespace => [ ] ),
            'arguments' => [ $$<exp> ],
          ) }
    | \( <?opt_ws> <exp> <?opt_ws> \)
        { return $$<exp> }   # ( exp )
    | \( <?opt_ws> <pair> <?opt_ws> \) 
        # special case - just for testing
        { return ::Lit::Pair( key => ($$<pair>)[0], value => ($$<pair>)[1] ) }
    | \{ <?opt_ws> <exp_mapping> <?opt_ws> \}
        { return ::Lit::Hash( 'hash' => $$<exp_mapping> ) }   # { exp => exp, ... }
    | \[ <?opt_ws> <exp_seq> <?opt_ws> \]
        { return ::Lit::Array( 'array' => $$<exp_seq> ) }     # [ exp, ... ]

    # Capture
    | \\ \( <?opt_ws> <capture> <?opt_ws> \)
        { return $$<capture> }                                # \( exp, ... )
    | \\ \( <?opt_ws> <exp_seq> <?opt_ws> \)
        { return ::Capture( 'invocant' => undef, 'array' => $$<exp_seq>, 'hash' => [ ] ); }
    | \\ <var>
        { return ::Capture( 'invocant' => undef, 'array' => [ $$<var> ], 'hash' => [ ] ); }

    | \$ \< <sub_or_method_name> \>
        { return ::Lookup( 
            'obj'   => ::Var( 'sigil' => '$', 'twigil' => '', 'name' => '/', namespace => [ ] ), 
            'index' => ::Val::Buf( 'buf' => $$<sub_or_method_name> ) 
        ) }   # $<ident>
    | do <?opt_ws> <block1>
        # block1 is defined in the Grammar::Control module
        { return ::Do( 'block' => $$<block1> ) }
    | use <?ws> <full_ident> <use_from_perl5> [ - <ident> | <''> ]
        { return ::Use( 'mod' => $$<full_ident>,'perl5' => $$<use_from_perl5> ) }
    | <val>      { return $$<val> }     # 'value'
    | <lit>      { return $$<lit> }     # [literal construct]
#   | <bind>     { return $$<bind>   }  # $lhs := $rhs
    
    | <token_sym>  
                 { return $$<token_sym>  }  # token:sym<...>  { regex... }
    | <token>    { return $$<token>      }  # token  { regex... }
    | <token_P5> { return $$<token_P5>   }  # token :P5 { regex... }

    | <proto>    { return $$<proto>  }  # proto  { code... }
    | <multi_method> 
                 { return $$<multi_method> }  # multi method { code... }
    | <method>   { return $$<method> }  # method { code... }
    | <multi_sub> 
                 { return $$<multi_sub> }  # multi { code... }
    | <subset>                          # subset x of y where { code... }
        { 
            if ($$<subset>).name ne '' {
                # subset x ...  -->  our &x ::= subset ...
                my $bind := ::Bind(  
                    parameters => ::Proto(  
                        name   => ($$<subset>).name,  
                    ),  
                    arguments => ::Subset( 
                        name       => '',  
                        base_class => ($$<subset>).base_class,
                        block      => ($$<subset>).block, 
                    ),
                );
                COMPILER::begin_block( $bind );   # ::=   compile-time
                return $bind;                         # :=    run-time
            };
            return $$<subset>;
        }  
        
    | <opt_declarator> <sub>               # my? sub xxx? { code... }
        { 
            if ($$<sub>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    return $$<sub>;
                } else {
                    print "Error: subroutines with declarators should have a name";
                    die "Error: subroutines with declarators should have a name";
                };
            };
            my $decl;
            if ($$<opt_declarator>) eq '' {
                $decl := 'our';
            } else {
                $decl := $$<opt_declarator>;
            };
            (@COMPILER::PAD[0]).add_lexicals( [
                ::Decl(  
                    decl  => $decl,  
                    var   => ::Var(  
                        name   => ($$<sub>).name,  
                        twigil => '',  
                        sigil  => '&', 
                        namespace => [ ],
                    ),  
                    type  => '', 
                ),
            ] );
            my $bind := ::Bind(  
                parameters => ::Var(  
                    name   => ($$<sub>).name,  
                    twigil => '',  
                    sigil  => '&', 
                    namespace => [ ],
                ),  
                arguments => $$<sub>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }  

    | <opt_declarator> <macro>               # my? macro xxx? { code... }
        { 
            if ($$<macro>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    return $$<macro>;
                } else {
                    print "Error: macros with declarators should have a name";
                    die "Error: macros with declarators should have a name";
                };
            };
            my $decl;
            if ($$<opt_declarator>) eq '' {
                $decl := 'our';
            } else {
                $decl := $$<opt_declarator>;
            };
            (@COMPILER::PAD[0]).add_lexicals( [
                ::Decl(  
                    decl  => $decl,  
                    var   => ::Var(  
                        name   => ($$<macro>).name,  
                        twigil => '',  
                        sigil  => '&', 
                        namespace => [ ],
                    ),  
                    type  => '', 
                ),
            ] );
            my $bind := ::Bind(  
                parameters => ::Var(  
                    name   => ($$<macro>).name,  
                    twigil => '',  
                    sigil  => '&', 
                    namespace => [ ],
                ),  
                arguments => $$<macro>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }  

    | <opt_declarator> <coro>               # my? coro xxx? { code... }
        { 
            if ($$<coro>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    return $$<coro>;
                } else {
                    print "Error: subroutines with declarators should have a name";
                    die "Error: subroutines with declarators should have a name";
                };
            };
            my $decl;
            if ($$<opt_declarator>) eq '' {
                $decl := 'our';
            } else {
                $decl := $$<opt_declarator>;
            };
            (@COMPILER::PAD[0]).add_lexicals( [
                ::Decl(  
                    decl  => $decl,  
                    var   => ::Var(  
                        name   => ($$<coro>).name,  
                        twigil => '',  
                        sigil  => '&', 
                        namespace => [ ],
                    ),  
                    type  => '', 
                ),
            ] );
            my $bind := ::Bind(  
                parameters => ::Var(  
                    name   => ($$<coro>).name,  
                    twigil => '',  
                    sigil  => '&', 
                    namespace => [ ],
                ),  
                arguments => $$<coro>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }  

    | <declarator> <?ws> <opt_type> <?opt_ws> <undeclared_var>   # my Int $variable
        { 
            if ($$<declarator>) eq 'my' {
                (@COMPILER::PAD[0]).add_lexicals( [
                    ::Decl( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> ),
                ] );
                return $$<undeclared_var>;
            };
            if ($$<declarator>) eq 'our' {
                # TODO - bind to namespace
                #say 'our declaration in namespace: ', (@COMPILER::PAD[0]).namespace;
                (@COMPILER::PAD[0]).add_lexicals( [
                    ::Decl( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> ),
                ] );
                return $$<undeclared_var>;
            };
            # TODO - our, temp, state, has
            return ::Decl( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> );
        }
    | <begin_block> 
                { return $$<begin_block> }  # BEGIN { code... }
    | <check_block> 
                { return $$<check_block> }  # CHECK { code... }

    | is <?ws> <full_ident> 
        { die "<is> not implemented" }
    | does <?ws> <full_ident> 
        { die "<does> not implemented" }

    | <control> { return $$<control> } # Various control structures.  Does _not_ appear in binding LHS
#   | <index>     # $obj[1, 2, 3]
#   | <lookup>    # $obj{'1', '2', '3'}
    | <apply>   { return $$<apply>  }  # self; print 1,2,3
    | \<  <angle_quoted>  \>
        { return ::Apply(
            'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'qw', namespace => [ ] ),
            'arguments' => [ ::Val::Buf( buf => ~$<angle_quoted> ) ],
          ) }

};
}
