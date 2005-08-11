#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests nothingmuch's C<lazy> proposal.

=cut

plan 3;

{
  my $was_in_lazy;

  my $var = lazy { $was_in_lazy++; 42 };

  ok !$was_in_lazy,     'our lazy block wasn\'t yet executed';
  is $var,          42, 'our lazy var has the correct value';
  ok $was_in_lazy,      'our lazy block was executed';
}
