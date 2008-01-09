
use v6-alpha;

grammar KindaPerl6::Grammar {

token term {
    | '...'
        { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'die', namespace => [ ] ),
            'arguments' => [],
          ) }
    | Inf  <!before <.word> | _ | <.digit> >
        { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'Inf', namespace => [ ] ),
            'arguments' => [],
          ) }
    | NaN  <!before <.word> | _ | <.digit> >
        { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'NaN', namespace => [ ] ),
            'arguments' => [],
          ) }

    | <var>     { make $$<var> }     # $variable
    | <arrow_sub> { make $$<arrow_sub> }     # -> $param { code... }
    | <prefix_op> <exp>
          { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<' ~ $<prefix_op> ~ '>', namespace => [ ] ),
            'arguments' => [ $$<exp> ],
          ) }

    # Parenthesis, List
    | \( <.opt_ws>
        [

        | <exp> <.opt_ws> \)
            { make $$<exp> }   # ( exp )
        | <pair> <.opt_ws> [ ',' <.opt_ws> | '' ] \)
            # special case - just for testing
            { make Lit::Pair.new( key => ($$<pair>)[0], value => ($$<pair>)[1] ) }
        | <exp_seq> <.opt_ws> \)
            { return
                Call.new(
                    'invocant'  => Proto.new( name => 'List' ),
                    'hyper'     => '',
                    'method'    => 'new',
                    'arguments' => [ Lit::Array.new( 'array' => $$<exp_seq> ) ]
                );
            }
        ]

    # Pair, Hash, Bare block
    | \{ <.opt_ws>
        [
        |   <pair> <.opt_ws> , <.opt_ws> \}
            { make Lit::Pair.new( key => ($$<pair>)[0], value => ($$<pair>)[1] ) }
        |   <pair> <.opt_ws> \}
            {
                make Lit::Code.new(
                    pad   => COMPILER::current_pad(),
                    state => { },
                    sig   =>
                        Sig.new(
                            'invocant' => undef,
                            'positional' => [ ],
                        ),
                    body  => [
                            Lit::Pair.new( key => ($$<pair>)[0], value => ($$<pair>)[1] )
                        ],
                );
            }
        |   <exp_mapping> <.opt_ws> \}
            { return
                Call.new(
                    'invocant'  => Proto.new( name => 'Hash' ),
                    'hyper'     => '',
                    'method'    => 'new',
                    'arguments' => [ Lit::Hash.new( 'hash' => $$<exp_mapping> ) ]
                );
            }
        |   <bare_block>
            {
                make $$<bare_block>;
            }
        |
            {
                die "syntax error inside bare block";
            }
        ]

    | \[ <.opt_ws> <exp_seq> <.opt_ws> \]
            { return
                Call.new(
                    'invocant'  => Proto.new( name => 'Array' ),
                    'hyper'     => '',
                    'method'    => 'new',
                    'arguments' => [ Lit::Array.new( 'array' => $$<exp_seq> ) ]
                );
            }

    # Capture
    | \\ \( <.opt_ws> <capture> <.opt_ws> \)
        { make $$<capture> }                                # \( exp, ... )
    | \\ \( <.opt_ws> <exp_seq> <.opt_ws> \)
        { make Lit::Capture.new( 'invocant' => undef, 'array' => $$<exp_seq>, 'hash' => [ ] ); }
    | \\ <var>
        { make Lit::Capture.new( 'invocant' => undef, 'array' => [ $$<var> ], 'hash' => [ ] ); }

    | \$ \< <sub_or_method_name> \>
        { make Call.new(
            'invocant'   => Var.new( 'sigil' => '$', 'twigil' => '', 'name' => '/', namespace => [ ] ),
            'hyper' => '',
            'method' => 'LOOKUP',
            'arguments' => [::Val::Buf( 'buf' => $$<sub_or_method_name> )]
        ) }   # $<ident>
    | do <.opt_ws> <block1>
        # block1 is defined in the Grammar::Control module
        { make Do.new( 'block' => $$<block1> ) }
    | use <.ws> <full_ident> <use_from_perl5> [ - <ident> | '' ]
        { make Use.new( 'mod' => $$<full_ident>,'perl5' => $$<use_from_perl5> ) }
    | <val>      { make $$<val> }     # 'value'
    | <lit>      { make $$<lit> }     # [literal construct]
#   | <bind>     { make $$<bind>   }  # $lhs := $rhs

    | <token_sym>
                 { make $$<token_sym>  }  # token:sym<...>  { regex... }
    | <token>    { make $$<token>      }  # token  { regex... }
    | <token_P5> { make $$<token_P5>   }  # token :P5 { regex... }

    | <proto>    { make $$<proto>  }  # proto  { code... }
    | <multi_method>
                 { make $$<multi_method> }  # multi method { code... }
    | <method>   { make $$<method> }  # method { code... }
    | <multi_sub>
                 { make $$<multi_sub> }  # multi { code... }
    | <subset>                          # subset x of y where { code... }
        {
            if ($$<subset>).name ne '' {
                # subset x ...  -->  our &x ::= subset ...
                my $bind := Bind.new(
                    parameters => Proto.new(
                        name   => ($$<subset>).name,
                    ),
                    arguments => Lit::Subset.new(
                        name       => '',
                        base_class => ($$<subset>).base_class,
                        block      => ($$<subset>).block,
                    ),
                );
                COMPILER::begin_block( $bind );   # ::=   compile-time
                make $bind;                         # :=    run-time
            };
            make $$<subset>;
        }

    | <opt_declarator> <sub>               # my? sub xxx? { code... }
        {
            if ($$<sub>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    make $$<sub>;
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
            ( COMPILER::current_pad() ).add_lexicals( [
                Decl.new(
                    decl  => $decl,
                    var   => Var.new(
                        name   => ($$<sub>).name,
                        twigil => '',
                        sigil  => '&',
                        namespace => [ ],
                    ),
                    type  => '',
                ),
            ] );
            my $bind := Bind.new(
                parameters => Var.new(
                    name   => ($$<sub>).name,
                    twigil => '',
                    sigil  => '&',
                    namespace => [ ],
                ),
                arguments => $$<sub>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }

    | <opt_declarator> <macro>               # my? macro xxx? { code... }
        {
            if ($$<macro>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    make $$<macro>;
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
            ( COMPILER::current_pad() ).add_lexicals( [
                Decl.new(
                    decl  => $decl,
                    var   => Var.new(
                        name   => ($$<macro>).name,
                        twigil => '',
                        sigil  => '&',
                        namespace => [ ],
                    ),
                    type  => '',
                ),
            ] );
            my $bind := Bind.new(
                parameters => Var.new(
                    name   => ($$<macro>).name,
                    twigil => '',
                    sigil  => '&',
                    namespace => [ ],
                ),
                arguments => $$<macro>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }

    | <opt_declarator> <coro>               # my? coro xxx? { code... }
        {
            if ($$<coro>).name eq '' {
                if ($$<opt_declarator>) eq '' {
                    make $$<coro>;
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
            ( COMPILER::current_pad() ).add_lexicals( [
                Decl.new(
                    decl  => $decl,
                    var   => Var.new(
                        name   => ($$<coro>).name,
                        twigil => '',
                        sigil  => '&',
                        namespace => [ ],
                    ),
                    type  => '',
                ),
            ] );
            my $bind := Bind.new(
                parameters => Var.new(
                    name   => ($$<coro>).name,
                    twigil => '',
                    sigil  => '&',
                    namespace => [ ],
                ),
                arguments => $$<coro>
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }

    | <declarator> <.ws> <opt_type> <.opt_ws> <undeclared_var>   # my Int $variable
        {
            if ($$<declarator>) eq 'my' {
                ( COMPILER::current_pad() ).add_lexicals( [
                    Decl.new( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> ),
                ] );
                make $$<undeclared_var>;
            };
            if ($$<declarator>) eq 'our' {
                # TODO - bind to namespace
                #say 'our declaration in namespace: ', ( COMPILER::current_pad() ).namespace;
                ( COMPILER::current_pad() ).add_lexicals( [
                    Decl.new( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> ),
                ] );
                make $$<undeclared_var>;
            };
            # TODO - our, temp, state, has
            make Decl.new( 'decl' => $$<declarator>, 'type' => $$<opt_type>, 'var' => $$<undeclared_var> );
        }
    | <begin_block>
                { make $$<begin_block> }  # BEGIN { code... }
    | <check_block>
                { make $$<check_block> }  # CHECK { code... }
    | gather <.ws> \{ <.opt_ws> <bare_block>      # gather { code... }
        { return
            Call.new(
                hyper     => '',
                arguments => [
                    Sub.new(
                        'name'  => undef,
                        'block' => $$<bare_block>,
                    ),
                ],
                method   => 'new',
                invocant => Proto.new( name => 'Gather', ),
            );
        }
    | is <.ws> <full_ident>
        { die "<is> not implemented" }
    | does <.ws> <full_ident>
        { die "<does> not implemented" }

    | <control> { make $$<control> } # Various control structures.  Does _not_ appear in binding LHS
#   | <index>     # $obj[1, 2, 3]
#   | <lookup>    # $obj{'1', '2', '3'}

    | <apply>   { make $$<apply>  }  # self; print 1,2,3
    | \<  <angle_quoted>  \>
        { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'qw', namespace => [ ] ),
            'arguments' => [ Val::Buf.new( buf => ~$<angle_quoted> ) ],
          ) }

};
}

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
