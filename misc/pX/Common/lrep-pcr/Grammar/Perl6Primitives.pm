package main;
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
