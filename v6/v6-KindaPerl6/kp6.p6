module Main {
    use KindaPerl6::Runtime::Perl5::Runtime;
    use KindaPerl6::Grammar;
    use KindaPerl6::Traverse;
    use KindaPerl6::Ast;
    use KindaPerl6::Grammar::Regex;
    use KindaPerl6::Runtime::Perl6::Compiler;

    my @visitors;
    @visitors.push('ExtractRuleBlock');
    #, 'Token', 'MetaClass', 'Global', 'EmitPerl5');
    
}
