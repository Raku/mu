grammar MyGrammar {
    token always_true {
        <''>
    };
    token myTok {
        <'a'> <always_true> <'b'> <always_true> <'c'>
    };
};
module Main {
    say '1..1';
    $_='abc';
    if MyGrammar.myTok() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};
