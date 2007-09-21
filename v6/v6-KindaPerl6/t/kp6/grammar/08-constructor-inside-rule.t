grammar MyGrammar {
    token myrule {
        '1abc'
        { return ::Ok(a => 'ok 1') }
    }
};
class Ok {
    has $.a;
};
module Main {
    say '1..1';

    $_ = '1abc';
    my $match = MyGrammar.myrule();
    my $result = $$match;
    say $result.a;
}
