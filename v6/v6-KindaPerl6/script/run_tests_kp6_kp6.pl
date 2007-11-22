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
  local $Test::Harness::Switches = "$Test::Harness::Switches -Icompiled/perl5-kp6-kp6/lib";
  $ok &&= eval { runtests glob("t/p5/*.t") };
  warn $@ if $@;
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X script/run_kp6_kp6_perl5.pl -Icompiled/perl5-kp6-kp6/lib";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  $ok &&= eval { runtests (glob("t/kp6-kp6/*.t"),glob("t/kp6-kp6/*/*.t")) };
}

if (defined $section)
{ # kp6-perl5.pl tests
  my $perl5 = $ENV{HARNESS_PERL} || $^X;
  local $ENV{HARNESS_PERL} = "$^X script/run_kp6_kp6_perl5.pl -Icompiled/perl5-kp6-kp6/lib";
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
  local $ENV{HARNESS_PERL} = "$^X script/run_kp6_kp6_perl5.pl -Ilib-modules-kp6-mp6-p5 -Icompiled/perl5-kp6-kp6/lib";
  local $ENV{PERL5LIB} = '';
  local $Test::Harness::Switches = '';
  open(TESTS,"TESTS") || die "Can not open test list";
  $ok &&= eval { runtests((map {chomp;"../../t/$_" } <TESTS>),glob("t/kp6/*.t"),glob("t/kp6/*/*.t")) };
}

if (!$ok) {
  print STDERR "some tests failed\n";
  exit 1;
}


=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
