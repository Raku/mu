use strict;
use warnings;

#use Smart::Comments;
use Test::More 'no_plan';

use Pugs::Compiler::Regex;

$Pugs::Compiler::Regex::NoCache = 1;

# Test the compile method:

# default values for the options
{
    my $regex = Pugs::Compiler::Regex->compile('a');
    is $regex->{grammar}, 'Pugs::Grammar::Base',
        "grammar => 'Pugs::Grammar::Base'";
    is $regex->{continue}, 0, "continue => 0";
    is $regex->{p}, undef, 'pos => undef';
    is $regex->{ratchet}, 0, 'ratchet => 0';
    is $regex->{ignorecase}, 0, 'ignorecase => 0';
    is $regex->{sigspace}, 0, 'sigspace => 0';
}

# test param handling
my $regex;
eval {
    $regex = Pugs::Compiler::Regex->compile(
        'a',
        { ratchet => 1, pos => 1, p => 1, sigspace => 1, s => 1,
        continue => 1, c => 1, ignorecase => 1, i => 1,
        grammar => 'abc' }
    );
};
is $@, '', 'params okay (1)';
ok $regex, 'regex ok (1)';
is $regex->{grammar}, 'abc',
    "grammar => 'abc'";
is $regex->{continue}, 1, "continue => 1";
is $regex->{p}, 1, 'pos => 1';
is $regex->{ratchet}, 1, 'ratchet => 1';
is $regex->{ignorecase}, 1, 'ignorecase => 1';
is $regex->{sigspace}, 1, 'sigspace => 1';

eval {
    $regex = Pugs::Compiler::Regex->compile(
        'a',
        { ratchet => 0, pos => 0, p => 0, sigspace => 0, s => 0,
        continue => 0, c => 0, ignorecase => 0, i => 0,
        grammar => '' }
    );
};
is $@, '', 'params okay (0)';
ok $regex, 'regex ok (0)';

eval {
    Pugs::Compiler::Regex->compile('a');
};
is $@, '', 'params are optional';
ok $regex, 'regex ok (null)';

# Test the code method
{
  $regex = Pugs::Compiler::Regex->compile('a|b', { ratchet => 1 });
  my $sub = $regex->code();
  my $match = $sub->('MyGrammar', 'aaa');
  is $match->(), 'a';
}

{
  package Foo;
  no strict;
  no warnings;

  my $regex = Pugs::Compiler::Regex->compile('a*');
  ### CODE: $regex->code
  *match = $regex->code();
  my $match = Foo->match('aaa');
  ### MATCH: $match
  ::is $match->(), 'aaa';
}

# Test the install method
{
  package Bar;
  Pugs::Compiler::Regex->install('match', 'a*', {ratchet => 1});
  my $match = Bar->match('aaa');
  ::is $match->(), 'aaa';
  eval {
    Pugs::Compiler::Regex->install('match', 'b*');
  };
  ::like $@, qr/Can't install regex 'match' as 'Bar::match' which already exists/, 'croak as expected';
}

# Test the reinstall method
{
  package Bar;
  Pugs::Compiler::Regex->reinstall('match', 'a*', {ratchet => 1});
  my $match = Bar->match('aaa');
  ::is $match->(), 'aaa';
  eval {
    Pugs::Compiler::Regex->reinstall('match', 'b*');
  };
  ::ok !$@, "shouldn't croak this time";
  $match = Bar->match('bbb');
  ::is $match->(), 'bbb', 'reinstall works';
}

# Test the continue option in ratchet mode
{
    package Foo;
    Pugs::Compiler::Regex->install(
        word => '\w+', { ratchet => 1, continue => 1 }
    );
    my $s = 'hello world';
    my @match;
    my $match = Foo->word($s);
    push @match, $match if $match;
    $match = Foo->word($s);
    push @match, $match if $match;
    $match = Foo->word($s);
    push @match, $match if $match;
    #while (my $match = Foo->word($s)) {
    #    push @match, $match->();
    #}
    ::is join(':', @match), 'hello:world';
}

{
    package Bar;
    Pugs::Compiler::Regex->install(
        digit => '\d', { ratchet => 1, c => 1 }
    );
    my $s = '56';
    my @match;
    my $match = Bar->digit($s);
    push @match, $match->() if $match;
    $match = Bar->digit($s);
    push @match, $match->() if $match;
    $match = Bar->digit($s);
    push @match, $match->() if $match;
    #while (my $match = Bar->digit($s)) {
    #    push @match, $match->();
    #}
    ::is join(':', @match), '5:6';
}

TODO: {
    local $TODO = ":c modifier doesn't work in non-ratchet mode";
    package Bar;
    Pugs::Compiler::Regex->reinstall(
        digit => '\d', { ratchet => 0, c => 1 }
    );
    my $s = '56';
    my @match;
    my $match = Bar->digit($s);
    push @match, $match->() if $match;
    $match = Bar->digit($s);
    push @match, $match->() if $match;
    $match = Bar->digit($s);
    push @match, $match->() if $match;
    #while (my $match = Bar->digit($s)) {
    #    push @match, $match->();
    #}
    package main;
    is join(':', @match), '5:6';
}

# Test match:
{
    my $regex = Pugs::Compiler::Regex->compile(
        'a*\w',
    );
    my $match = $regex->match('aaa');
    ok $match->bool, 'backtracking works';
    is $match->(), 'aaa';
    is "$match", 'aaa';
    is $match->from, 0;
    is $match->to, 3;
}

# Test the :pos modifier
#{
#}

