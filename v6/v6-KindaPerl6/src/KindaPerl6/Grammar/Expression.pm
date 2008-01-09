use v6-alpha;

grammar KindaPerl6::Grammar {

token infix_op {
      '+' | '-' | '*' | '//' | '/' | eq | ne | '==' | '!=' | '&&' | '||' | '~~' | '~'
    | '<=>'
    | '<=' | '>=' 
    | '<'  | '>' 
    | '&' | '^' | '|'
    | '..'
    | 'but'
    | 'x'
};

token hyper_op {
    '>>' | ''
};

token prefix_op {
    [ '$' | '@' | '%' | '?' | '!' | '++' | '--' | '+' | '-' | '~' | '|' ] 
    <before '$' | '@' | '%' 
          | '(' | '{' | '[' 
    >
};

# Lowest Precedence

    # See: <term>

# Middle Precedence

token term_meth {
    <full_ident>
    [ <.dot>
        <hyper_op>
        <ident>
            [ \( <.opt_ws> <exp_parameter_list> <.opt_ws> \)
                # { say 'found parameter list: ', $<exp_parameter_list>.perl }
            | \: <.ws> <exp_parameter_list> <.opt_ws>
            |
                {
                    make Call.new(
                        'invocant'  => Proto.new( 'name' => ~$<full_ident> ),
                        'method'    => $$<ident>,
                        'arguments' => undef,
                        'hyper'     => $$<hyper_op>,
                    )
                }
            ]
            {
                make Call.new(
                    'invocant'  => Proto.new( 'name' => ~$<full_ident> ),
                    'method'    => $$<ident>,
                    'arguments' => $$<exp_parameter_list>,
                    'hyper'     => $$<hyper_op>,
                )
            }
    ]
    |
    <term>
    [ 
    | [ '[' | '.[' ]  <.opt_ws> <exp> <.opt_ws> \]   # $a[exp]
         { make Call.new(
                 'invocant' => $$<term>,
                 'arguments' => [$$<exp>],
                 'method' => 'INDEX',
                 'hyper' => '' 
           )
         }
    | [ '{' | '.{' ] <.opt_ws> <exp> <.opt_ws> \}   # $a{exp}
         { make Call.new(
                 'invocant' => $$<term>,
                 'arguments' => [$$<exp>],
                 'method' => 'LOOKUP',
                 'hyper' => ''
           )
         }
    | <.dot>
        <hyper_op>
        <opt_ident>   # $obj.(42)
            [ \( 
                # { say 'testing exp_parameter_list at ', $/.to }
                <.opt_ws> <exp_parameter_list> <.opt_ws> \)
                # { say 'found parameter list: ', $<exp_parameter_list>.perl }
            | \: <.ws> <exp_parameter_list> <.opt_ws>
            |
                {
                    make Call.new(
                        'invocant'  => $$<term>,
                        'method'    => $$<opt_ident>,
                        'arguments' => undef,
                        'hyper'     => $$<hyper_op>,
                    )
                }
            ]
            {
                make Call.new(
                    'invocant'  => $$<term>,
                    'method'    => $$<opt_ident>,
                    'arguments' => $$<exp_parameter_list>,
                    'hyper'     => $$<hyper_op>,
                )
            }
    | \< <angle_quoted> \>   # $a{exp}
         { make Call.new(
                 'invocant' => $$<term>,
                 'arguments' => [ Val::Buf.new( 'buf' => ~$<angle_quoted> ) ],
                 'method' => 'LOOKUP',
                 'hyper' => ''
           )
         }
    |    { make $$<term> }
    ]
};

# Lowest Precedence

token exp2 { <exp> { make $$<exp> } };

token exp {
    # { say 'exp: going to match <term_meth> at ', $/.to; }
    <term_meth> 
    [
        <.opt_ws>
        '??'
        [
          <.opt_ws>  <exp>
          <.opt_ws>  
                [ '::' { die "maybe you mean ternary:<?? !!>" } 
                | '!!' 
                ]
          <.opt_ws>
          <exp2>
          { 
          
            # XXX TODO - expand macro
            # is &ternary:<?? !!> a macro?
            my $macro_ast := Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] );
            my $macro := COMPILER::get_var( $macro_ast );
            if defined($macro) {
                # fetch the macro 
                my $sub := ( COMPILER::current_pad() ).eval_ast( $macro_ast );
                Main::expand_macro( $sub, $$<term_meth>, $$<exp>, $$<exp2> );
                # say "# ternary macro = ", $sub.perl;
            }
            
            make Apply.new(
                'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] ),
                'arguments' => [ $$<term_meth>, $$<exp>, $$<exp2> ],
            ); 
          }
        | { say '*** Syntax error in ternary operation' }
        ]
    |
        <.opt_ws>
        <infix_op>
        <.opt_ws>
        <exp>
          { make Apply.new(
            'code'      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<' ~ $<infix_op> ~ '>', namespace => [ ]  ),
            'arguments' => [ $$<term_meth>, $$<exp> ],
          ) }
    | <.opt_ws> '::=' <.opt_ws> <exp>
        { 
            my $bind := Bind.new( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>);
            COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }
    | <.opt_ws> ':=' <.opt_ws> <exp>
        { make Bind.new( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    | <.opt_ws> '=' <.opt_ws> <exp>
        { make Assign.new( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    |   { make $$<term_meth> }
    ]
};


}

=begin

=head1 NAME 

KindaPerl6::Grammar - Expression Grammar for KindaPerl6

=head1 SYNOPSIS

    my $match := $source.parse;
    ($$match).perl;    # generated KindaPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the KindaPerl6 compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2007 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
