#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests nothingmuch's C<lazy> proposal.

This proposal was accepted on 2005-08-23 in a p6l post by Larry
(http://www.nntp.perl.org/group/perl.perl6.language/22890):

> Which already seems to be there with
> 
>     lazy {...}
> 
> which is, I presume, mostly syntactic sugar for something like:
> 
>     sub is cached {...}

=cut

plan 6;

{
  my $was_in_lazy;

  my $var = lazy { $was_in_lazy++; 42 };

  ok !$was_in_lazy,     'our lazy block wasn\'t yet executed (1)';
  is $var,          42, 'our lazy var has the correct value';
  ok $was_in_lazy,      'our lazy block was executed';
}

# Same, but passing the lazy value around before accessing it:
{
  my $was_in_lazy;

  my $var = lazy { $was_in_lazy++; 42 };
  my $sub = -> Num $v, Bool $access { $access and +$v };

  ok !$was_in_lazy,         'our lazy block wasn\'t yet executed (2)';
  $sub($var, 0);  
  ok !$was_in_lazy,         'our lazy block has still not been executed', :todo<unspecced>;
  $sub($var, 1);
  ok $was_in_lazy,          'our lazy block has been executed now';
}

# Arguably, we should remove the $was_in_lazy tests, as it doesn't really
# matter when the lazy {...} block is actually executed.
