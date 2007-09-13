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

    COMPILER::env_init();

    # this should be used importing
    # Digest::MD5::md5_hex from perl5;
    $COMPILER::source_md5 = 'temporary_value';

    my $pos = 0;

    #while ($pos < length($code)) {
        my $ast = KindaPerl6::Grammar.comp_unit($code, $pos);
        if (!($ast.isa('CompUnit'))) {
            die 'Syntax Error!';
        };
        for @visitors -> $visitor {
            $ast.emit($visitor);
        };
        print $ast;
        $pos = $pos + $/.to;
    #}

}
