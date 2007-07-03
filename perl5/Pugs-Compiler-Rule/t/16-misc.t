use Test::More tests => 6;

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

