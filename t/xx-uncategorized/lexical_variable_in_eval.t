use v6-alpha;
use Test;
=qwid

This
  my $str = "abc";
  eval_ok(q{$str == "abc"});
was resulting in
  eval was fatal: Undeclared variable: "$str"

=cut

plan 1;

my $str = "abc";
eval_ok(q{$str == "abc"},'$str works in eval_ok - bug fixed.');
