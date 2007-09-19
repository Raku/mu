grammar MyGrammar {
    token tok1 {
        'a' { return Array.new(); };
    };
    token tok2 {
        <tok1> { return $$<tok1>; };
    };
};
module Main {
    say '1..1';
    $_ = 'a';
    if MyGrammar.tok2() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};