grammar MyGrammar {
    token myrule {
        '1abc'
        { return Ok.new(result => 'ok 1') }
    }
};
class Ok {
    has $.result;
    method str {
        return $.result;
    };
    method true {
        return 1;
    };
};
module Main {
    say '1..1';

    $_ = '1abc';
    say MyGrammar.myrule();
}
