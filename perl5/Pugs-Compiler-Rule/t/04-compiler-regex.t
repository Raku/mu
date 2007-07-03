use strict;
use warnings;

#use Smart::Comments;
use Test::More 'no_plan';

use Pugs::Compiler::Regex;

$Pugs::Compiler::Regex::NoCache = 1;

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

