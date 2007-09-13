grammar MyGrammar {
    token mytoken  { 'a' | 'b' };
};
module Main {
    $_ = '';
    say '1..1';
    if (MyGrammar.mytoken()) {
        say 'not ok 1';
    } else {
        say 'ok 1';
    }
}