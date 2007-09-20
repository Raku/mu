grammar MyGrammar {
    token myrule {
       <'1'> <ident1> .
    }

    token ident1 {
      <before 'a'> <'ab'>
    };

    token ident3 {
      <!before \"> . <ident3> | <''>
    };
    token ident2 {
      \" <ident3> \"
    };
};
module Main {
    say '1..3';

    $_ = '1abc';
    if MyGrammar.myrule() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
    $_ = '1ab';
    if MyGrammar.ident1($_, 1) {
        say 'ok 2';
    } else {
        say 'not ok 2';
    }
    $_ = '"abc"';
    if MyGrammar.ident2() {
        say 'ok 3';
    } else {
        say 'not ok 3';
    }
}
