grammar MyGrammar {
    token opt_ws  { ' ' | '' };
};
module Main {
    say '1..2';
    $_ = ' ';
    if MyGrammar.opt_ws() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    };

    $_ = '';
    if MyGrammar.opt_ws() {
        say 'ok 2';
    } else {
        say 'not ok 2';
    }
    
};
