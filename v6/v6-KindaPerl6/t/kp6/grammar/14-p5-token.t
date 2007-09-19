grammar MyGrammar {
    token word :P5 {[[:word:]]};
};
module Main {
    say '1..1';
    $_ = 'a';
    if MyGrammar.word() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};