#!/usr/bin/env perl

use strict;
use warnings;
use Test::Harness;
use Getopt::Long;

#$Test::Harness::Debug = 1;
$Test::Harness::Verbose = 1 if $ENV{TEST_VERBOSE};

my $section = undef;my $backend = undef;
GetOptions( "section:s" => \$section,"backend:s" => \$backend);

my $ok = 1;
if (not defined $backend) {
    die "You must specify a backend.\n";
}
if (defined $section)
{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X script/kp6 -B$backend";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests(glob("t/kp6/$section/*.t")) };
  warn $@ if $@;
}
else # all
{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  warn $@ if $@;
  local $ENV{HARNESS_PERL} = "$^X script/kp6 -B$backend";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests((map {chomp;"../../t/$_" } <TESTS>),glob("t/kp6/*.t"),glob("t/kp6/*/*.t")) };
}

if (!$ok) {
  print STDERR "some tests failed\n";
  exit 1;
}
