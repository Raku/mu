#!/usr/bin/pugs

use v6;

# Please remember to update t/examples/examples.t and rename
# examples/output/junctions/grades if you rename/move this file.

my $threshold = 70;

sub authorize_movie(@grade) {
  if (one(@grade) < $threshold) {
    say 'you may go to the movies';
  } else {
    say "sorry you had more than one grade below $threshold";
  }
}

sub load_grades {
  qw(50 80 90);
}

my @grade = load_grades();
authorize_movie(@grade);


