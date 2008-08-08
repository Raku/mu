use strict;
use warnings;

use Test::More 'no_plan';
use Pugs::Compiler::Rule;
use Pugs::Grammar::Base;
use Pugs::Runtime::Match;

$Pugs::Compiler::Regex::NoCache = 1;

{
    my $rule = Pugs::Compiler::Rule->compile('a');
    ok $rule, 'rule obj ok';
    isa_ok $rule, 'Pugs::Compiler::Regex';
    is $rule->{ratchet}, 1, 'ratchet defaults to 1';
    is $rule->{sigspace}, 1, 'sigspace defaults to 1';
    is $rule->{ignorecase}, 0, 'ignorecase defaults to 0';
    is $rule->{grammar}, 'Pugs::Grammar::Base', 'grammar no overridden';
    is $rule->{p}, undef, 'p no overridden';
    is $rule->{continue}, 0, 'continue no overridden';
}

# test ratchet and sigspace
{
    my $rule = Pugs::Compiler::Rule->compile(
        'a*\w',
    );
    my $match = $rule->match('aaa');
    ok !$match->bool, 'no backtracking';
}
{
    my $rule = Pugs::Compiler::Rule->compile(
        'a b',
    );
    my $match = $rule->match('a     b');
    ok $match->bool, 'sigspace works';
    is $match->(), 'a     b', 'sigspace works (2)';
}

# override ratchet and sigspace with undef values:
{
    my $rule = Pugs::Compiler::Rule->compile(
        'a*\w =',
        { ratchet => undef, sigspace => undef }
    );
    is $rule->{ratchet}, 0, 'ratchet defaults to 1';
    is $rule->{sigspace}, 0, 'sigspace defaults to 1';
    is $rule->{ignorecase}, 0, 'ignorecase defaults to 0';
    is $rule->{grammar}, 'Pugs::Grammar::Base', 'grammar no overridden';
    is $rule->{p}, undef, 'p no overridden';
    is $rule->{continue}, 0, 'continue no overridden';

    my $match = $rule->match('aaa=');
    ok $match->bool, 'backtracking works';
    is $match->(), 'aaa=', 'capture ok';

    $match = $rule->match('aaa =');
    ok !$match->bool, 'sigspace => 0';
}

