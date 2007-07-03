use Test::More tests => 2;

use Pugs::Compiler::Regex;

# test param handling
eval {
    my $regex = Pugs::Compiler::Regex->compile(
        'a',
        { ratchet => 1, p => 1, sigspace => 1, s => 1,
        continue => 1, c => 1, ignorecase => 1, i => 1,
        grammar => 'abc' }
    );
};
is $@, '';

eval {
    my $regex = Pugs::Compiler::Regex->compile(
        'a',
        { ratchet => 0, p => 0, sigspace => 0, s => 0,
        continue => 0, c => 0, ignorecase => 0, i => 0,
        grammar => '' }
    );
};
is $@, '';

