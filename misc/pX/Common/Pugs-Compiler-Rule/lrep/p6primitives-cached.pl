# generated file - do not edit!
package grammar1;
*{'statement_control:<if>'} = sub {
    my $rule = ruleop::concat( 
        ruleop::constant( 'statement_control:<if>' ),
        \&grammar1::ws_star,
       ruleop::concat(
         ruleop::optional(
             \&{'grammar1::ws'}
           ,
         )
       ,
         ruleop::constant( "\(" )
       ,
         ruleop::capture( 'expr', 
             ruleop::non_greedy_star(
                 \&{'grammar1::any'}
               ,
             )
           ,
         )
       ,
         ruleop::constant( "\)" )
       ,
         ruleop::optional(
             \&{'grammar1::ws'}
           ,
         )
       ,
         ruleop::capture( 'block', 
             ruleop::capture( 'code', \&{'grammar1::code'} )
           ,
         )
       ,
       )
    );
    my $match = $rule->( @_ );
    return unless $match;
    my $code = sub { 
    my $src = <<'!EOT!'; 
{
    return '
        sub prefix:<_if_expr>  { return $expr ; }
        sub prefix:<_if_block> { $block }
        eval( \' 
                if ( &{\\\'prefix:<_if_expr>\\\'}() ) { 
                    &{\\\'prefix:<_if_block>\\\'}() 
                } 
            \', 
            :lang<perl5> );
    ';
}
!EOT!
    my $block = match::str( match::get( $_[0], '$<block>' ) );
    $block =~ s/([\'\\])/\\$1/g;

    my $expr = match::str( match::get( $_[0], '$<expr>' ) );
    $expr =~ s/([\'\\])/\\$1/g;

    $src =~ s/([\'"\\])/\\$1/g;
    my $ret = eval( '"' . $src . '"' ); 
    die $@ if $@; 
    my $ast = grammar1::immediate_statement_rule( $ret );
    die "compile: syntax error in macro at '" . $ast->{tail} . "'\n"
        if $ast->{tail};
    my $perl5 = Perl6Grammar::emit( $ast->{capture} );
    my $expanded = eval $perl5;
    die $@ if $@; 
    my $final_ast = 
        ::compile_rule( q( [ <?ws>? <@grammar1::statements> ]* <?ws>? ) )
        ->( $expanded );
    die "compile: syntax error in macro at '" . $final_ast->{tail} . "'\n"
        if $final_ast->{tail};
    return $final_ast;
    };
    my $ast = $code->( $match ); 
    return { %$match, capture => [ $ast->{capture} ] }; 
};
    push @grammar1::statements, \&{'statement_control:<if>'};
    *{'infix:<*>'} = sub
    {
 $_[0] * $_[1]     }
    ;
    push @grammar1::ops, ::compile_rule( 'infix\:\<\*\>' );
    *{'infix:<+>'} = sub
    {
 $_[0] + $_[1]     }
    ;
    push @grammar1::ops, ::compile_rule( 'infix\:\<\+\>' );
    *{'infix:<~>'} = sub
    {
 $_[0] . $_[1]     }
    ;
    push @grammar1::ops, ::compile_rule( 'infix\:\<\~\>' );
