knowhow EXTERNAL {
    my $p5;
    sub use_from_perl5($module) {
        unless $p5 {
            $p5 := ::P5Interpreter.new;
        }
        $p5.eval(PRIMITIVES::idconst_concat('use ',$module.FETCH));
        $p5.eval(PRIMITIVES::idconst_concat(PRIMITIVES::idconst_concat("'",$module.FETCH),"'"));
    }
};
$LexicalPrelude.{'EXTERNAL::'} := ::EXTERNAL.^!who.FETCH;
