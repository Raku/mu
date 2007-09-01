grammar MyGrammar {

    token myrule {
        . <ident>
    }

    token ident {
      <!before \d>
      <'abc'>
    };
};
module Main {
    say '1..1';

    $_ = '1abc';
    if MyGrammar.myrule() {
        say 'not ok 1';
    } else {
        say 'ok 1';
    }
}
