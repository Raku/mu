my @pacific    = qw(800 720 600 511);
@pacific   = qw(715 550 411);

my @atlantic   = qw(400 420 300 211);

if (all(@pacific) > all(@atlantic)) {
  say "every team in division 1 has a higher win percentage that division 2";
} else {
  say "some team in division 2 is outdoing a division 1 team";
}
