use v6-alpha;

grammar KindaPerl6::Grammar {

token method_sig {
    |   <.opt_ws> \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { make $$<sig> }
    |   { make Sig.new(
            'invocant' => Var.new(
                'sigil'  => '$',
                'twigil' => '',
                'name'   => 'self',
                'namespace' => [ ],
                ),
            'positional' => [ ],
            ) }
};
token sub_sig {
    |   <.opt_ws> \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { make $$<sig> }
    |   { make Sig.new(
            'invocant' => undef,
            'positional' => [ ],
            ) }
};

token arrow_sub_sig {
    |   <exp_sig_list>
        { make Sig.new(
            'invocant' =>   Val::Undef.new(),
            'positional' => $$<exp_sig_list>,
            ) }
    |   \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { make $$<sig> }
}

token sub {
    sub
    <.ws>  <opt_name>  <.opt_ws>
    <sub_sig>
    <.opt_ws> \{
        <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in sub \'', $$<name>, '\': missing closing curly bracket '; die 'error in Block'; } ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );
        make Sub.new(
            'name'  => $$<opt_name>,
            'block' => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token coro {
    coro
    <.ws>  <opt_name>  <.opt_ws>
    <sub_sig>
    <.opt_ws> \{
        <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in coro \'', $$<name>, '\': missing closing curly bracket '; die 'error in Block'; } ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );
        make Coro.new(
            'name'  => $$<opt_name>,
            'block' => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token arrow_sub {
    '->'
    <.opt_ws>
    <arrow_sub_sig>
    <.opt_ws> \{
        <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in sub: missing closing curly bracket  '; die 'error in Block'; } ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<arrow_sub_sig>).positional).>>key,
            ]
        );
        make Sub.new(
            'name'  => undef,
            'block' => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => $$<arrow_sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token bare_block {
    # used by gather { ... }
    # \{ <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in Block: missing closing curly bracket  '; die 'error in Block'; } ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                # Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                # @(($$<sub_sig>).positional).>>key,
            ]
        );
        make Lit::Code.new(
                pad   => $env,
                state => { },
                sig   =>
                    Sig.new(
                        'invocant' => undef,
                        'positional' => [ ],
                    ),
                body  => $$<exp_stmts>,
            );
    }
};

token proto {
    proto <.ws> [ [ method | token | rule | sub ] <.ws> | '' ]
        <namespace> <ident> <.ws> '{' <.opt_ws> '}'
        {
            # proto token x { }
            my $bind := Bind.new(
                parameters =>         # no pre-declaration checks ???
                        Var.new(
                            sigil     => '&',
                            twigil    => '',
                            name      => ~$<ident>,
                            namespace => $$<namespace>,
                        ),
                arguments =>
                        Call.new(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'new',
                            'invocant'  => Proto.new(
                                name => 'Multi',
                            ),
                        ),
            );
            #COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }
}

token method {
    method
    <.ws>  <opt_name>  <.opt_ws>
    <method_sig>
    <.opt_ws> \{ <.opt_ws>
        # { say ' parsing statement list ' }
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        # { say ' got statement list ', ($$<exp_stmts>).perl }
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;

        my $env   := COMPILER::current_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                ($$<method_sig>).invocant,
                @(($$<method_sig>).positional).>>key,
            ]
        );
        COMPILER::drop_pad();
        make Method.new(
            'name'  => $$<opt_name>,
            'block' => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => $$<method_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token multi_method {
    multi <.ws> method

            # multi method { code... }
            # &multi.add_variant(
            #    method ($a,$b,$c,$d) {
            #        say 'ok 4';
            #    }
            # );

        <.ws>  <namespace> <ident>  <.opt_ws>
        <method_sig>
        <.opt_ws> \{ <.opt_ws>
            {
                COMPILER::add_pad();
            }
            <exp_stmts>
            <.opt_ws>
        [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;

            my $env   := COMPILER::current_pad();
            KindaPerl6::Grammar::declare_parameters(
                $env,
                [
                    Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                    ($$<method_sig>).invocant,
                    @(($$<method_sig>).positional).>>key,
                ]
            );
            COMPILER::drop_pad();
            return
                Call.new(
                    hyper     => '',
                    method   => 'add_variant',
                    invocant => Var.new(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),
                    arguments => [
                        Method.new(
                            name  => '',
                            'block' => Lit::Code.new(
                                pad   => $env,
                                state => { },
                                sig   => $$<method_sig>,
                                body  => $$<exp_stmts>,
                            ),
                        ),
                    ],
                );
        }

}

token multi_sub {
    multi <.ws> [ sub <.ws> | '' ]

        <namespace> <ident>  <.opt_ws>
        <sub_sig>
        <.opt_ws> \{ <.opt_ws>
            {
                COMPILER::add_pad();
            }
            <exp_stmts>
            <.opt_ws>
        [   \}     | { say '*** Syntax Error in sub \'', get_class_name(), ' ', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;

            my $env   := COMPILER::current_pad();
            KindaPerl6::Grammar::declare_parameters(
                $env,
                [
                    Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                    # ($$<sub_sig>).invocant,
                    @(($$<sub_sig>).positional).>>key,
                ]
            );
            COMPILER::drop_pad();
            return
                Call.new(
                    hyper     => '',
                    method   => 'add_variant',
                    invocant => Var.new(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),
                    arguments => [
                        Sub.new(
                            name  => '',
                            'block' => Lit::Code.new(
                                pad   => $env,
                                state => { },
                                sig   => $$<sub_sig>,
                                body  => $$<exp_stmts>,
                            ),
                        ),
                    ],
                );
        }
}

token token {
    # { say 'parsing Token' }
    token
    <.ws>  <opt_name>  <.opt_ws> \{
        <KindaPerl6::Grammar::Regex.rule>
    \}
    {
        make Token.new(
            name  => ~$<opt_name>,
            regex => $$<KindaPerl6::Grammar::Regex.rule>,
            sym   => undef,
        );
    }
};

token token_sym_ident {

    # TODO - process whitespace in angle_quoted and french_quoted strings

    |  sym \< <angle_quoted>  \>    { make ~$<angle_quoted> }
    |  sym \« <french_quoted> \»    { make ~$<french_quoted> }
    |  <ident>                      { make ~$<ident> }
}

token token_sym {
    # { say 'parsing Token:sym' }
    [ multi <.ws> | '' ]
    token
    <.ws> <namespace> <ident> \: <token_sym_ident> <.opt_ws> \{
        <KindaPerl6::Grammar::Regex.rule>
    \}
    {
            return
                Call.new(
                    hyper     => '',
                    method   => 'add_token_variant',
                    invocant => Var.new(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),
                    arguments => [
                        Token.new(
                            name  => undef,
                            regex => $$<KindaPerl6::Grammar::Regex.rule>,
                            sym   => ~$<token_sym_ident>,
                        ),
                        Val::Buf.new( 'buf' => ~$<token_sym_ident> ),
                    ],
                );
    }
};


token macro {
    macro
    <.ws>  <opt_name>  <.opt_ws>
    <sub_sig>
    <.opt_ws> \{
        <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    [   \}     | { say '*** Syntax Error in macro \'', $$<name>, '\''; die 'error in Block'; } ]
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                Var.new( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );
        make Macro.new(
            'name'  => $$<opt_name>,
            'block' => Lit::Code.new(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};


};

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
