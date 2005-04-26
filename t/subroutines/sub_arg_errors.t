#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

These are misc. sub argument errors.

=cut

sub foo (*$x) { 1 }
dies_ok  { foo(reverse(1,2)) }, 'slurpy args are now bounded (1)';

sub bar (*@x) { 1 }
lives_ok { bar(reverse(1,2)) }, 'slurpy args are now bounded (2)';
