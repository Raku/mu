# any compared with any

my @first_set = qw(1 1 1 1 1 1 1 1 1 1 1 1 1 1);
my @new_set = qw(1 2 1 1 1 1 1 1 1);

if (any(@first_set) != any(@new_set)) {
  "a fluctuation in the readings has been detected".say;
}
