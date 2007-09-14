grammar MyGrammar {
    token to_line_end {
        |  <'a'> <?to_line_end>
        |  <''> 
    };
};
module Main {
    say '1..1';
    $_ = 'aaaaaaaaaaaa';
    if MyGrammar.to_line_end() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
}
