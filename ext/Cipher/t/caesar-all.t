#!/usr/bin/pugs
# Tests of Cipher::Caesar in rotate-all mode

use Test;
use Cipher::Caesar;

my $encrypt = Cipher::Caesar.new(:all, :shift(32), :mode<enciphering>);
my $decrypt = Cipher::Caesar.new(:all, :shift(32), :mode<deciphering>);
plan(5);

is($encrypt.cipher('TEST'), 'test', "Alphabetics happily encrypt across case boundaries");
is($decrypt.cipher('test'), 'TEST', "...and back again.");
is($encrypt.cipher('#@!?'), 'C`A_', "Non-alphabetics rotate");
is($decrypt.cipher("\n"  ), "\xea", "Rotates backwards across null boundary");
is($encrypt.cipher("\xea"),   "\n", "...and forward again");
