#!/usr/bin/pugs

use v6;

=pod

Clearing 'has'-attrib hashes with "= ()" messes them up; subsequent
hash access fails.

This happens to both private and public members.

=cut

class HashCrash;

use Test;
plan 2;

has %.pubhash;
has %:privhash;

method run_test() {
    lives_ok { %.pubhash  = (); %.pubhash<1>  = 1 }, "%.hash = () works", :todo<bug>;
    lives_ok { %.privhash = (); %.privhash<1> = 1 }, "%:hash = () works", :todo<bug>;
}

HashCrash.new.run_test;

