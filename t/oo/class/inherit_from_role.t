#!/usr/bin/pugs

use v6;
use Test;

=pod
    I am not sure about what will ::?CLASS return.
    Hope what I did is correct.

=cut

plan 4;

role A01 { method test { "$?CLASS" } };
class B01 does A01 { };
is B01.new.test, "B01", 'Testing for the inherited $?CLASS value from a role.', :todo<feature>;

class A02 { method test { "$?CLASS" } };
class B02 is A02 { };
is B02.new.test, "B02", 'Testing for the inherited $?CLASS value from a class.', :todo<feature>;


role A03 { method test { "{::?CLASS}" } };
class B03 does A03 { };
is B03.new.test, "B03", 'Testing for the inherited ::?CLASS value from a role.', :todo<feature>;

class A04 { method test { "{::?CLASS}" } };
class B04 is A04 { };
is B04.new.test, "B04", 'Testing for the inherited ::?CLASS value from a class.', :todo<feature>;




