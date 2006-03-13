package Grammar::Perl6;
*{'statement_control:<if>'} = sub {
    my $rule = Runtime::Perl5::RuleOps::concat( 
        Runtime::Perl5::RuleOps::constant( 'statement_control:<if>' ),
        \&Grammar::Perl6::ws_star,
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::capture( 'expr', 
             Runtime::Perl5::RuleOps::non_greedy_star(
                 \&{'Grammar::Perl6::any'}
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'block', 
             Runtime::Perl5::RuleOps::capture( 'code', \&{'Grammar::Perl6::code'} )
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
    my $ast = Grammar::Perl6::immediate_statement_rule( $ret );
    die "compile: syntax error in macro at '" . $ast->{tail} . "'\n"
        if $ast->{tail};
    my $perl5 = Emitter::Perl5::emit( $ast->{capture} );
    my $expanded = eval $perl5;
    die $@ if $@; 
    require Runtime::Perl5::RuleInit;
    my $final_ast = 
        Runtime::Perl5::RuleOps::compile_rule( q( [ <?ws>? <@Grammar::Perl6::statements> ]* <?ws>? ) )
        ->( $expanded );
    die "compile: syntax error in macro at '" . $final_ast->{tail} . "'\n"
        if $final_ast->{tail};
    return $final_ast;
    };
    my $ast = $code->( $match ); 
    return { %$match, capture => [ $ast->{capture} ] }; 
};
#endblock
    push @Grammar::Perl6::statements, \&{'statement_control:<if>'};
    *{'infix:<*>'} = sub

    {
 $_[0] * $_[1] 
    }
    ;
    require Runtime::Perl5::RuleInit;
    push @Grammar::Perl6::ops, Runtime::Perl5::RuleOps::compile_rule( 'infix\:\<\*\>' );
    *{'infix:<+>'} = sub

    {
 $_[0] + $_[1] 
    }
    ;
    require Runtime::Perl5::RuleInit;
    push @Grammar::Perl6::ops, Runtime::Perl5::RuleOps::compile_rule( 'infix\:\<\+\>' );
    *{'infix:<~>'} = sub

    {
 $_[0] . $_[1] 
    }
    ;
    require Runtime::Perl5::RuleInit;
    push @Grammar::Perl6::ops, Runtime::Perl5::RuleOps::compile_rule( 'infix\:\<\~\>' );
