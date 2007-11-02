deliberate death;

# this test case will not complete (infinite loop?).
# I am breaking this code so that the test case still fails.
# test cases
#      t/kp6/grammar/15-capture-from-rule-block.t
#      t/kp6/grammar/17-complex-return.t
#
# are both broken with what appears to be the same bug?
# dlocaus@irc.freenode.net #perl6


grammar MyGrammar {
    token tok1 {
        'a' { return Array.new(); }
    };
    token tok2 {
        <tok1> { return $$<tok1>; }
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
