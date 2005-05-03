# Demo of the state() variable declaration.

use v6;

sub gen_cashier {
    state $cash_in_store = 0;
    return {
        add => { $cash_in_store += @_[0] },
        del => { $cash_in_store -= @_[0] },
        bal => { $cash_in_store          },
    }; 
}

my $drawer;
for 1 .. 3 { $drawer.{$_} = gen_cashier() }

$drawer.{1}<add>( 59 );
$drawer.{2}<del>( 17 );
say $drawer.{3}<bal>();
