#!/usr/bin/perl6

use v6;

=pod

=head1 Valid Number

You want to check if a string is a valid number.

# XXX pugs currently ignores types for everything but MMD.

  # Most of the time you will not need to do this.  Rather then testing
  # for a scalar's numerical nature you can ensure that the variable
  # contains a number by setting its type.  Assigning a number to
  # that variable will cause it to be coerced into an integer or a
  # number.

  # Ensure that a variable is used to store a real number. 

  my Num $number;

  # Ensure that a variable is used to store an integer.

  my Int $integer;

  # Sometimes you need to validate a string from some source
  # corresponds to a real or an integer.  In this situation compare
  # it against the rule for integers or reals.

# XXX When Perl6 becomes self hosting then we can test it against the rules
# that Perl uses until then we will have to use these.

  rule natural {<digit>+};

  rule integer {[\+|\-]? <natural>};

  rule decimal { [ <integer> [\. <natural>]? ] | [ [\+|\-]? \. <natural> ] };

  rule cfloat  { <decimal> [ [e|E] <integer>]? }

  given $string {
    say "Natural" if /^<natural>$/;
    say "Integer" if /^<integer>$/;
    say "Decimal" if /^<decimal>$/;
    say "Cfloat"  if /^<cfloat>$/;
  }

=cut

# XXX 

