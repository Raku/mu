#!/usr/bin/pugs

use v6;
use Test;

plan 1;

role A { method test { "$?CLASS" } };
class B does A { };

is B.new.test, "B", 'Testing for the inherited $?CLASS value from a role.';



