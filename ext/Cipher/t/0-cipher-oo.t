#!/usr/bin/pugs

use v6;
use Test;
use Cipher::TestGuts;

my $ec1 = new Cipher::TestGuts;
my $ec2 = Cipher::TestGuts.new(:mode<encrypting>);
my $dc1 = Cipher::TestGuts.new(:mode<decrypting>);

plan(12);
is($ec1.mode, "enciphering", "mode check 1");
is($ec2.mode, "enciphering", "mode check 2");
is($dc1.mode, "deciphering", "mode check 3");

is($ec1.cipher("foo"), "headE1E1E1", "enciphering 1");
is($ec1.cipher("foo"), "E2E2E2", "1 keeps state");
is($ec2.num_invocations, 0, "2 not affected by 1");
is($ec2.cipher("foo"), "headE1E1E1", "2 ciphers properly");
is($dc1.cipher("foo"), "headD1D1D1", "deciphering 1");

is($ec1.finishstr(), "tail", "finishstr encipherer 1");
is_deeply($dc1.finish(), [0x74,0x61,0x69,0x6c], "finish decipherer 1");

is($ec2.num_invocations, 1, "sanity check");
$ec2.zeroize();
is($ec2.num_invocations, 0, "zeroization works");
