#!/usr/bin/pugs

use v6;
use Test;

# Test primarily aimed at PIL2JS

plan 5;

{
  my $res;

  for <a b c> { $res ~= $_ }
  is $res, "abc", "for works with an <...> array literal";
}

{
  my $res;

  for (<a b c>) { $res ~= $_ }
  is $res, "abc", "for works with an (<...>) array literal";
}

{
  my $res;

  for ("a",) { $res ~= $_ }
  is $res, "a", "for works with an (a_single_constant,) array literal";
}

{
  my $res;

  for ("a") { $res ~= $_ }
  is $res, "a", "for works with (a_single_constant)";
}

{
  my $res;

  for "a" { $res ~= $_ }
  is $res, "a", "for works with \"a_single_constant\"";
}
