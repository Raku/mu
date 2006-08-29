use v6-alpha;

use Test;

=kwid

while statement tests

L<S04/"Loop statements">

=cut

plan 11;

{
  my $i = 0;
  while $i < 5 { $i++; };
  is($i, 5, 'while $i < 5 {} works');
}
{
  my $i = 0;
  while 5 > $i { $i++; };
  is($i, 5, 'while 5 > $i {} works');
}
# with parens
{
  my $i = 0;
  while ($i < 5) { $i++; };
  is($i, 5, 'while ($i < 5) {} works');
}
{
  my $i = 0;
  while (5 > $i) { $i++; };
  is($i, 5, 'while (5 > $i) {} works');
}

# single value
{
  my $j = 0;
  while 0 { $j++; };
  is($j, 0, 'while 0 {...} works');
}
{
  my $k = 0;
  while $k { $k++; };
  is($k, 0, 'while $var {...} works');
}

# L<S04/The C<for> statement/It is also possible to write>
# while ... -> $x {...}
{
  my @array = 0..5;
  my $str;
  eval_ok q{
      while @array.shift -> $x {
          $str ~= $x
      }
  };
  is $str, '012345', 'while ... -> $x {...} worked (1)';
}

{
  my @array = 0..5;
  my $str;
  eval_ok q{
      while shift @array -> $x {
          $str ~= $x;
      }
  };
  is $str, '012345', 'while ... -> $x {...} worked (2)';
}
