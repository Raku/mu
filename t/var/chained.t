#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# sanity: declarations and very simple use (scoping tests come later)
# we take care to use different names to avoid other *kinds* of insanity.

is((try {              my $a1 = my    $b1 = 42; $b1++; ($a1, $b1) }).perl, '(\\42, \\43)', "chained my");
is((try {              my $a2 = our   $b2 = 42; $b2++; ($a2, $b2) }).perl, '(\\42, \\43)', "chained my, our");
is((try {              my $a3 = let   $b3 = 42; $b3++; ($a3, $b3) }).perl, '(\\42, \\43)', "chained my, let");
is((try {              my $a4 = env   $b4 = 42; $b4++; ($a4, $b4) }).perl, '(\\42, \\43)', "chained my, env");
is((try {              my $a5 = state $b5 = 42; $b5++; ($a5, $b5) }).perl, '(\\42, \\43)', "chained my, state");
is((try { my $b6 = 10; my $a6 = temp  $b6 = 42; $b6++; ($a6, $b6) }).perl, '(\\42, \\43)', "chained my, temp");

# scoping

is((try {
    my $sa1 = 10;
    {
        my $sa1 = our $sb1 = 42;
    }
    ($sa1, $sb1);
   }).perl, '(\\10, \\42)', "scoping my, our");

dies_ok {
    {
        our $sa2 = my $sb2 = 42;
    }
    ($sa2, $sb2);
   } "scoping our, my ('our' doesn't leak)";

# XXX: add more!
