use v6-alpha;

use Test;

plan 3;

# Hmm.  Tricky since crypt() is implemented via system call and
# will not necessarily be the same on all platforms.

# Note: most crypt() only look at first 8 chars  of key

my $pw1 = crypt '12345678', 'XX';
my $pw2 = crypt '12345678', 'XX';

# Not a good test since crypt might be memoizing somewhere  but
# gotta start with something.
is $pw1, $pw2, 'basic test of repeatability';

my $pw3 = crypt '12345678', 'XY';

# True test would test all salts but the size of that task
# is why the salt exists to begin with.
isnt $pw1, $pw3, 'different salts give different results';

my $pw4 = crypt '12345679', 'XX';

isnt $pw1, $pw4, 'different key give different results';
