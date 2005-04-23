#!/usr/bin/pugs

use v6;
require Test;

=kwid

Context tests.

=cut

plan 2;

sub foo (*$x) { 1 }
dies_ok  { foo reverse(1,2) }, "signature '*\$x' works";

sub bar (*@x) { 1 }
lives_ok { bar reverse(1,2) }, "signature '*\@x' works";
