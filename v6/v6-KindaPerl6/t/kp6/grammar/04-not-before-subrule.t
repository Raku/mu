grammar MyGrammar {
    token ident {
      <!before \d>
      <?word>
    };
};
module Main {
    say '1..1';
    say 'ok 1';
}
