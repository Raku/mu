=pod

This recipe is the recipe6 from the Perl5 Moose OO system, just
syntactically adapted to Perl6.

See
http://search.cpan.org/~stevan/Moose/lib/Moose/Cookbook/Recipe6.pod
for more description or feel free to cut'n'paste it here.

=cut


role Eq {

  method equal_to { ... }

  method not_equal_to ($other) {
    not $.equal_to($other);
  }
}

role Comparable {

  does Eq;
  method compare { ... }

  method equal_to ($other) {
    $.compare($other) == 0;
  }

  method greater_than ($other) {
    $.compare($other) == 1;
  }

  sub less_than ($other) {
    $.compare($other) == -1;
  }

  sub greater_than_or_equal_to ($other) {
    $.greater_than($other) || $.equal_to($other);
  }

  sub less_than_or_equal_to ($other) {
    $.less_than($other) || $.equal_to($other);
  }
}

role Printable {
  method to_string { ... }
}

class US::Currency {

  does Comparable;
  does Printable;

  has Num $.amount is rw = 0;

  method compare ($other) {
    $.amount <=> $other.amount
  }

  method to_string {
    sprintf '$%0.2f USD', $.amount
  }
}

# --- main ---

my $my_money     = new US::Currency;
$my_money.amount = 9876.50;
my $your_money   = US::Currency.new( amount => 1000 );
say $my_money.to_string; # $9876.50 USD
say "m'kay!" if $my_money.greater_than($your_money);
