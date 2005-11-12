#!/usr/bin/pugs

use v6;
use Test;

=pod
    access or assgin on undeclared attribute will raise an error.

=cut

plan 8;


dies_ok { class A { method a { $.a = 1 }}; A.new.a; },
    "Test Undeclared public attribute assignment from a class";
dies_ok { role B { method b { $.b = 1 }};class C does B{ }; C.new.b; },
    "Test Undeclared public attribute assignment from a role";

dies_ok { class D { method d { $:d = 1 }}; D.new.d; },
    "Test Undeclared private attribute assignment from a class";
dies_ok { role E { method e { $:e = 1 }};class F does E{ }; F.new.e; },
    "Test Undeclared private attribute assignment from a role";


##### access the undeclared attribute
dies_ok { class H { method h { $.h }}; H.new.h; },
    "Test Undeclared public attribute access from a class";
dies_ok { role I { method i { $.i }};class J does I{ }; J.new.i; },
    "Test Undeclared public attribute access from a role";

dies_ok { class K { method k { $:k }}; K.new.k; },
    "Test Undeclared private attribute access from a class";
dies_ok { role L { method l { $:l }};class M does L{ }; M.new.l; },
    "Test Undeclared private attribute access from a role";

