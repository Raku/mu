#!/usr/bin/pugs

use v6;
use Test;

=pod

Clearing 'has'-attrib hashes with "= ()" messes them up; subsequent
hash access fails.

This happens to both private and public

=cut

plan 2;

class HashCrash;

has %.pubhash;
has %:privhash;

method run_test() {
    lives_ok { %.pubhash  = (); %.pubhash<1>  = 1 }, "%.hash = () works";
    lives_ok { %.privhash = (); %.privhash<1> = 1 }, "%:hash = () works";
}

HashCrash.new.run_test;

