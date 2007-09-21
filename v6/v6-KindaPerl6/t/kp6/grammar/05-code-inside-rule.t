grammar MyGrammar {
    token myrule {
        '1abc'
        { return 'ok ' ~ (42 - 41) }
    }
};
module Main {
    say '1..1';

    $_ = '1abc';
    say $(MyGrammar.myrule());
}
