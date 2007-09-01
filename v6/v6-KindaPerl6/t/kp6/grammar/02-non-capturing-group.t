grammar MyGrammar {
    token ws { <' '> };
    token alpha { 'a' | 'b' };
    token opt_alfanum  { [ <?ws> | <?alpha> ] |  '' };
};
module Main {
    say '1..3';
    $_ = ' ';
    if MyGrammar.opt_alfanum() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    };

    $_ = '';
    if MyGrammar.opt_alfanum() {
        say 'ok 2';
    } else {
        say 'not ok 2';
    };

    $_ = 'a';
    if MyGrammar.opt_alfanum() {
        say 'ok 3';
    } else {
        say 'not ok 3';
    };
};
