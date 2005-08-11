#!/usr/bin/pugs

use v6;
use Test;

=pod

A conditional statement modifier that tries to avoid calling undef as
a coderef will instead trip over a pugs cast failure, but only if the
value is actually undef:

    pugs> my $a = sub { 1 }; my $b; sub c($code) { return 1 if $code and $code(); return 0 }
    undef
    pugs> c($a)
    1
    pugs> c($b)
    pugs: cannot cast from VUndef to Pugs.AST.Internals.VCode

This causes pugs to exit.

=cut

plan 2;

{
    my $a = sub { 1 };
    my $b;
    sub c($code) { return 1 if $code and $code(); return 0 }

    is c($a), 1, 'shortcircuit idiom given coderef works';

    # This one will just kill pugs with the cast failure, so force fail
    eval_ok '# c($b)', 'shortcircuit idiom given undef works', :todo<bug>;
}
