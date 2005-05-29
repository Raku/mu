#!/usr/bin/pugs
# Demo of the state() variable declaration.
# This is also a neat way of doing OO without actually having OO available.

use v6;

sub gen_cashier () {
    # This variable corresponds to a class variable.
    # It is shared across all "instances" of gen_cashier().
    state $cash_in_store = 0;

    # One could add my() variables here, which correspond to instance variables.
    # These would not be shared.

    # Finally, we return a hashref which maps method names to code.
    return {
        add => { $cash_in_store += $^amount },
        del => { $cash_in_store -= $^amount },
        bal => { $cash_in_store             },
    }; 
}

my $drawer;
$drawer[$_] = gen_cashier() for 1..3;

$drawer[1]<add>( 59 );
$drawer[2]<del>( 17 );
say $drawer[3]<bal>();  # This should say "42"
