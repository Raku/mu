grammar MyGrammar {
    token ident {
       <'infix:<'> <infix_op> <'>'>
       |
       [
          <!before \d>
          <?word>
         |
          _
       ]
       <?ident_digit>
    };
};
module Main {
    say '1..1';
    say 'ok 1';
}
