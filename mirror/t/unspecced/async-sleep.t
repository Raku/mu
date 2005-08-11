#!/usr/bin/pugs

use v6;
use Test;

=kwid

= DESCRIPTION

This test tests the interaction of C<async{}> and
C<sleep>. Ideally, while a C<sleep> call is in progress
in one thread, other C<async> blocks are still being
executed.

=cut

plan 1;
diag "Running under $*OS";

my @events;
my $event_count = 2;

sub spawn_counter () {
  async {
    my $count = $event_count;
    while ($count--) {
      push @events, time();
      sleep 1; # five seconds are enough for everybody
    };
  };
};

diag "This test will take about " ~ ($event_count+2) ~ " seconds.";

spawn_counter();
sleep $event_count+2;

if (!ok(@events == $event_count, "Our async counter finished while we were sleeping")) {
  diag "Got      " ~ +@events ~ " element(s).";
  diag "Expected $event_count elements."
};
