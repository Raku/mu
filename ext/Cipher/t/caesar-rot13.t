# Tests of Cipher::Caesar in rot13 mode

use Test;
use Cipher::Caesar;

my $rot13 = Cipher::Caesar.new;

plan(2);
is($rot13.cipher("test"), "grfg", "seems to work");
is($rot13.cipher("a\nb"), "n\no", "doesn't affect non-alphabetics");
