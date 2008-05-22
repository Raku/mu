use v6;

use Test;

plan 2;

=pod

# L<< S05/Return values from matches/"The array elements of a C<Match> object are referred to" >>

Testing capture rollback when an alternation fails.


=cut

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

{
  "abd" ~~ m/ (a) (b) c || (\w) b d /;
  is( @($/).elems, 1, "correct number of positional captures");
}

{
  "abd" ~~ m/ <alpha> <alpha> c || <alpha> b d /;
  flunk "skipping parsefail test", :todo<bug>;
  #is( @($/<alpha>).elems, 1, "correct number of named captures", :todo<bug>);
}

