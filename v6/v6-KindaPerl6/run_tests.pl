#!/usr/bin/env perl

use strict;
use warnings;
use Test::Harness;

#$Test::Harness::Debug = 1;
#$Test::Harness::Verbose = 1;

my $ok = 1;

{ # Perl5 tests
  # Use $ENV{HARNESS_PERL} or $^X
  local $Test::Harness::Switches = "$Test::Harness::Switches -Ilib5";
  $ok &&= eval { runtests glob("t/p5/*.t") };
  warn $@ if $@;
}

{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X run_kp6_perl5.pl -Ilib5";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests((map {chomp;"../../t/$_" } <TESTS>),glob("t/kp6/*.t")) };
  warn $@ if $@;
}

if (!$ok) {
  print STDERR "some tests failed\n";
  exit 1;
}
