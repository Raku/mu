module Main {
    use KindaPerl6::Runtime::Perl5::KP6Runtime;
    use KindaPerl6::Grammar;
    use KindaPerl6::Traverse;
    use KindaPerl6::Ast;
    use KindaPerl6::Grammar::Regex;
    use KindaPerl6::Runtime::Perl6::Compiler;
    use KindaPerl6::Runtime::Perl6::Grammar;

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

    COMPILER::env_init();

    # this should be used importing
    # Digest::MD5::md5_hex from perl5;
    $COMPILER::source_md5 = 'temporary_value';

    my $pos = 0;
    my $len = length $code;

    while ($len > $pos) {

        my $ast = KindaPerl6::Grammar.comp_unit($code, $pos);
        say 'Finished matching...';
        if (!($ast.isa('CompUnit'))) {
            die 'AST IS:(' ~ $ast ~ ')';
        };
        for @visitors -> $visitor {
            $ast.emit($visitor);
        };
        print $ast;
        $pos = $pos + $/.to;

    }

}
