grammar MyGrammar {
    token tok1 {
        'a' { say "closure";return 2; }
    };
}
module Main {
    say '1..2';
    MyGrammar.tok1('a',0);
    say 'ok 1';
    say (MyGrammar.tok1('a',0)).bool;
    if MyGrammar.tok1('a',0) {
        say 'ok 2';
    } else {
        say 'not ok 2';
    }
}
#reason:
#ShortCiruit turns return 1 && 2 into &infix:<&&>(sub {return 1},sub {2})
#fix: add -> {return ...} to kp6 and make ShortCircuit use ->
