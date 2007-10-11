#!/usr/bin/env perl

use strict;
use warnings;
use Test::Harness;
use Getopt::Long;

#$Test::Harness::Debug = 1;
$Test::Harness::Verbose = 1 if $ENV{TEST_VERBOSE};

my $section = undef;
GetOptions( "section:s" => \$section );

my $ok = 1;

unless (defined $section)
{ # Perl5 tests
  # Use $ENV{HARNESS_PERL} or $^X
  local $Test::Harness::Switches = "$Test::Harness::Switches -Ilib-kp6-kp6-p5 -Ilib-modules-kp6-kp6-p5";
  $ok &&= eval { runtests glob("t/p5/*.t") };
  warn $@ if $@;
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X run_kp6_kp6_perl5.pl -Ilib-kp6-kp6-p5 -Ilib-modules-kp6-kp6-p5";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  $ok &&= eval { runtests (glob("t/kp6-kp6/*.t"),glob("t/kp6-kp6/*/*.t")) };
}

if (defined $section)
{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X run_kp6_kp6_perl5.pl -Ilib-kp6-kp6-p5 -Ilib-modules-kp6-kp6-p5";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests(glob("t/kp6/$section/*.t")) };
  warn $@ if $@;
}
else # all
{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X run_kp6_kp6_perl5.pl -Ilib-kp6-kp6-p5 -Ilib-modules-kp6-kp6-p5";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests((map {chomp;"../../t/$_" } <TESTS>),glob("t/kp6/*.t"),glob("t/kp6/*/*.t")) };
  warn $@ if $@;
}

if (!$ok) {
  print STDERR "some tests failed\n";
  exit 1;
}
