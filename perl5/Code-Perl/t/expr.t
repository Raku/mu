# $Header: /home/fergal/my/cvs/Code-Perl/t/expr.t,v 1.4 2003/06/17 18:37:05 fergal Exp $

use strict;

use warnings;

use Test::More 'no_plan';

use lib 't';
use Code::Perl::Test::Expr;

$SIG{__DIE__} = $SIG{__WARN__} = \&Carp::confess;

use Code::Perl::Expr qw( :easy );

our $env;

my $chrs = join("", map {chr($_)} 0..127);

my @tests = (
    ['scalar', "value", scal("main::env"), "value", '$main::env'],
    ['number', "", number(10), 10, '10'],
    ['string', "", string("hello"), "hello", '"hello"'],
    ['string esc', "", string("\n\rhello\\\t"), "\n\rhello\\    ", "\"\\n\\rhello\\\\    \""],
    ['string all esc', "", string($chrs), $chrs, undef],
    ['list', "value", list(string("a"), number(10)), Code::Perl::Test::Expr::listcon("a", 10), '"a", 10'],
    [
        'deref hash auto',
        {a => "value"},
        derefh(scal("main::env"), "a"),
        "value",
        '($main::env)->{"a"}'
    ],
    [
        'deref hash',
        {a => "value"},
        derefh(scal("main::env"), string("a")),
        "value",
        '($main::env)->{"a"}'
    ],
    [
        'deref array auto',
        [1..10],
        derefa(scal("main::env"), 5),
        6,
        '($main::env)->[5]'
    ],
    [
        'deref array',
        [1..10],
        derefa(scal("main::env"), number(5)),
        6,
        '($main::env)->[5]'
    ],
    [
        'sub',
        2,
        calls("main::testsub", scal("main::env"), string("a")),
        "2, a",
        'main::testsub($main::env, "a")'
    ],
    [
        'sub dyn',
        \&testsub,
        calls(scal("main::env"), string("a"), string("b")),
        "a, b",
        '&{$main::env}("a", "b")'
    ],
    [
        'method',
        2,
        callm(string("main"), "testmethod", scal("main::env"), number(1)),
        "2, 1",
        '("main")->testmethod($main::env, 1)'
    ],
    [
        'method dyn',
        "testmethod",
        callm(string("main"), scal("main::env"), scal("main::env"), string("a")),
        "testmethod, a",
        '("main")->$main::env($main::env, "a")'
    ],
    ['boolnot 1', {}, boolnot(number(1)), Test::Deep::bool(0), "! (1)"],
    ['boolnot 0', {}, boolnot(number(0)), Test::Deep::bool(1), "! (0)"],
    ['perl', {}, perl("2 + 2"), 4, "2 + 2"],
    ['append', {}, append(string("a"), string("b"), string("c")), "abc", '("a") . ("b") . ("c")'],
    ['holder', {}, holder(perl("2 + 2")), 4, '2 + 2'],
    ['subname', {}, subname("hello"), Code::Perl::Test::Expr::do_not_run(), "hello"],
);

Code::Perl::Test::Expr::test_exprs(@tests);

sub testsub
{
    return main->testmethod(@_);
}

sub testmethod
{
    my $pkg = shift;
    my $a = shift;
    my $b = shift;
    return "$a, $b";
}

