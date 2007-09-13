module Main {
    use KindaPerl6::Runtime::Perl5::KP6Runtime;
    use KindaPerl6::Grammar;
    use KindaPerl6::Traverse;
    use KindaPerl6::Ast;
    use KindaPerl6::Grammar::Regex;
    use KindaPerl6::Runtime::Perl6::Compiler;

    my @visitors;
    @visitors.push('ExtractRuleBlock');
    @visitors.push('Token');
    @visitors.push('MetaClass');
    @visitors.push('Global');   
    @visitors.push('EmitPerl5');
    
    for @visitors -> $visitor {
        require 'KindaPerl6::Visitor::' ~ $visitor;
    }

    my $code = slurp;


}
