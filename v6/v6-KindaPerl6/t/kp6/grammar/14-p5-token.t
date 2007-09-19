grammar MyGrammar {
    token word :P5 {[[:word:]]};
};
module Main {
    say '1..3';
    $_ = 'a';
    if MyGrammar.word() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
    $_ = '';
    if MyGrammar.word() {
        say 'not ok 2';
    } else {
        say 'ok 2';
    }
    $_ = '1';
    if MyGrammar.word() {
        say 'ok 3';
    } else {
        say 'not ok 3';
    }
};