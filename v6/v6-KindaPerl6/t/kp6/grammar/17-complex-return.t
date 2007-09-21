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