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
            my $macro_ast := ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] );
            my $macro := COMPILER::get_var( $macro_ast );
            if defined($macro) {
                # fetch the macro 
                my $sub := ( COMPILER::current_pad() ).eval_ast( $macro_ast );
                Main::expand_macro( $sub, $$<term_meth>, $$<exp>, $$<exp2> );
                # say "# ternary macro = ", $sub.perl;
            }
            
            make ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] ),
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
          { make ::Apply(
            'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<' ~ $<infix_op> ~ '>', namespace => [ ]  ),
            'arguments' => [ $$<term_meth>, $$<exp> ],
          ) }
    | <.opt_ws> '::=' <.opt_ws> <exp>
        { 
            my $bind := ::Bind( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>);
            COMPILER::begin_block( $bind );   # ::=   compile-time
            make $bind;                         # :=    run-time
        }
    | <.opt_ws> ':=' <.opt_ws> <exp>
        { make ::Bind( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    | <.opt_ws> '=' <.opt_ws> <exp>
        { make ::Assign( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    |   { make $$<term_meth> }
    ]
};


}

=begin

=head1 NAME 

KindaPerl6::Grammar - Grammar for KindaPerl6

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
