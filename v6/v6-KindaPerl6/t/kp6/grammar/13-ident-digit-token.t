grammar MyGrammar {
    token word :P5 {[[:word:]]};
    token ident_digit {
          <?word> <?ident_digit>
        | <''>
    };
};
module Main {
    say '1..1';
    $_ = 'a123';
    if MyGrammar.ident_digit() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};