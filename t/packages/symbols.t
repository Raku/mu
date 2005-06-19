
use v6;
use Test;
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
    '%Package:: syntax', :todo<feature>;
isa_ok($sym, 'package', '%Package:: variable is a package',
       :todo<feature>);

# ... t.b.c. with correct lookup syntax
