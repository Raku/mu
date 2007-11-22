#!/usr/bin/env perl
# Run a kp6 script by filtering it through the kp6->perl5 compiler and
# run perl5 on the result
# The trick is getting arguments to the right people

use strict;
use warnings;

if (0) {
  local $" = ':';
  print STDERR "script/run_kp6_perl5.pl: got @ARGV\n";
}

# This is a poor heuristic
my $prog; # kp6 program
my @args = @ARGV; # arguments to perl5
for my $i (0..$#args) {
  # Test::Harness calls $perl -le "print join qq[\\n], \@INC
  # but we don't want to run this, so we just get out of here quick
  if ($args[$i] eq "-le" and $args[$i+1] =~ /\@INC/) {
    exit 0;
  }

  # find the first arg not prefixed with -
  # replace it with -
  if ($args[$i] !~ /^-/) {
    $prog = $args[$i];
    $args[$i] = '-';
  }
}

my $cmd = "$^X script/kp6 -r kp6 <$prog | $^X @args";
#print STDERR "script/run_kp6_perl5.pl: running: $cmd\n";
system $cmd;


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
