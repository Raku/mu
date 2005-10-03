#!pugs

use Test;
use Cipher::TestGuts;

plan(3);
is(Cipher::TestGuts.encipher("foo"), "headE1E1E1tail", "encipher works");
is(Cipher::TestGuts.encipher("foo"), "headE1E1E1tail", "invocations are separate");
is(Cipher::TestGuts.decipher("irr"), "headD1D1D1tail", "decipher works");
