my @new_quote = qw(7.5 6.5 5.5);
my $old_quote = 5;

if (any(@new_quote) < $old_quote) {
  "I'm sorry, but we are going to re-finance".say
}
