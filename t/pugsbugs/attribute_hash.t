#!/usr/bin/pugs

use v6;

=pod

Clearing 'has'-attrib hashes with "= ()" messes them up; subsequent
hash access fails.

This happens to both private and public members.

=cut

use Test;

class HashCrash;

# XXX - FIXME - Here we qualify the Test:: methods, because
# when precompiling Test.pm with prelude (see config.yml),
# export works differently and cause the plan() to be hidden
# (as currently the export only happens at parsing time, and
# precompilation inhibits the reparsing).
Test::plan 2;

has %.pubhash;
has %:privhash;

method run_test() {
    Test::lives_ok { %.pubhash  = (); %.pubhash<1>  = 1 }, "%.hash = () works", :todo<bug>;
    Test::lives_ok { %:privhash = (); %:privhash<1> = 1 }, "%:hash = () works", :todo<bug>;
}

HashCrash.new.run_test;

