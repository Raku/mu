#!/usr/bin/pugs

use v6;
use Test;

=kwid

= DESCRIPTION

This test tests the interaction of C<async{}> and
C<system>. Ideally, while a C<system> call is in progress,
C<async> blocks are still being executed.

=cut

plan 1;
diag "Running under $?OS";

my @events;
my $event_count = 4;

sub spawn_counter () {
  async {
    my $count = $event_count;
    while ($count--) {
      diag $count;
      push @events, time();
      sleep 1; # five seconds are enough for everybody
    };
  };
};

my ($pugs,$redir) = ("./pugs", ">");
my $todo = 0;

if($?OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  skip 1, "async known to be problematic on Win32";
  exit;
};

diag "Spawning counter";
spawn_counter();
system( qq!$pugs -e "sleep(5)"!); 

if (!ok(@events == $event_count, "Our async counter finished while we were running a subprocess", :todo($todo))) {
  diag "Got      " ~ +@events ~ " element(s).";
  diag "Expected $event_count elements."
};
