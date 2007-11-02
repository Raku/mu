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
    token t1 {
        'a' { return ::Foo( 'a' => 6 ); }
    };
    token t2 {
        <t1> 'b' { return ::Bar( 't1' => $$<t1>, 'b' => 'ok 4' ); }
    };
};
class Foo {
    has $.a;
};
class Bar {
    has $.t1;
    has $.b;
};
module Main {
    say '1..6';
    $_ = 'ab';
    my $match = MyGrammar.t2();
    if $match { say 'ok 1'; };
    my $result = $$match;
    if $result { say 'ok 2'; };
    if $result.isa('Bar') { say 'ok 3'; };
    say $result.b;
    my $t1 = $result.t1;
    if $t1 { say 'ok 5'; } else { say 'not ok 5' };
    say 'ok ' ~ $t1.a;
};
