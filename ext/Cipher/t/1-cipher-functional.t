#!/usr/bin/pugs

use v6;
use Test;
use Cipher::TestGuts;

my $ec1 = Cipher::TestGuts.encipherer();
my $ec2 = Cipher::TestGuts.encipherer();
my $dc1 = Cipher::TestGuts.decipherer();

plan(5);
is($ec1("foo"), "headE1E1E1", "works");
is($ec1("foo"), "E2E2E2", "carries state");
is($ec2("foo"), "headE1E1E1", "separate encipherers have separate state");
is($dc1("foo"), "headD1D1D1", "decipherers work");
is($ec1(), "tail", "tail routine invoked");

#clean these up quietly
$ec2();
$dc1();
