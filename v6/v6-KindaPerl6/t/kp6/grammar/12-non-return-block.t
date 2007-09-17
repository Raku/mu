grammar MyGrammar {
    token mytok {
        a
        { say "ok 1" }
        b
        { return "ok 2" }
    };
};
module Main {
    say '1..2';
    $_ = 'ab';
    say MyGrammar.mytok();
};
