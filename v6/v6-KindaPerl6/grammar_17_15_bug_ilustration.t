grammar MyGrammar {
    token tok1 {
        'a' { return 2; }
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
