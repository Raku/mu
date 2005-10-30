#!/usr/bin/pugs

use v6;
use Test;
plan 1;

# See thread "Packages, Modules and Classes" on p6l
# started by Stevan Little:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/23019">
skip_rest "test needs to be rewritten because of recent design changes";

=begin obsolete

plan 2;

# test the functions to browse the symbol table, and refer to packages
package Symbols::Galore {
    our $scalar = 42;
    our @array  = (1..13);
    our %hash   = ( here => "there" );
}

# so, how exactly are they specified?  hmm
my $sym;
lives_ok{ $sym = %Symbols::Galore:: },
    '%Package:: syntax';
isa_ok($sym, 'package', '%Package:: variable is a package',
       :todo<feature>);

# ... t.b.c. with correct lookup syntax
