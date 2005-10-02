#!/usr/bin/pugs
# Tests of Cipher::Caesar in Caesar mode

use Test;
use Cipher::Caesar;

# Julius Caesar really used this setting to communicate with his generals.  He 
# was lucky there weren't any competent cryptoanalysts in Gaul.
my $julius  = Cipher::Caesar.new(:shift(3));
my $general = Cipher::Caesar.new(:shift(3), :mode<decipher>);
plan(3);

is($julius .cipher("Et tu, Brute?"),  "Hw wx, Euxwh?",  "Encrypting works");
is($general.cipher("Dwwdfn dw gdzq"), "Attack at dawn", "So does decrypting");
is($general.cipher($julius.cipher("Test")), "Test", "Round-tripping works");
