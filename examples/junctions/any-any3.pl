# any compared with any - example 2

my @first_set = qw(1 1);
my @new_set = qw(1 1.4 1 1 1 1 1 1 0.8);

my $any_new_set = any(@new_set);
my $any_first_set = any(@first_set);

if ( (abs($any_first_set - $any_new_set)) > 0.5) {
  "a variation in the readings is too large".say;
}
