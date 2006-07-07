use v6-alpha;

use Test;

# L<"http://use.perl.org/~autrijus/journal/25337">
# my() declarations scopes lexically to the rest of the block; using $MY::x or
# $::("x") in the block before the actual declaration is erroneous.

plan 11;

{
  is(eval('my $x; my $x; 1'), 1, "test declare my() variable twice in same scope");
}

{
  is(eval('$x; my $x = 42'), undef, 'my() variable not yet visible (2)', :todo<bug>);
  is(eval('my $x = 42; $x'), 42, 'my() variable is visible now (2)');
}

{
  my $ret = 42;
  is(eval('$ret = $x ~ my $x; 1'), undef, 'my() variable not yet visible (1)', :todo<bug>);
  is $ret, 42,                   'my() variable not yet visible (2)', :todo<bug>;
}

{
  my $ret = 42;
  lives_ok { $ret = my($x) ~ $x }, 'my() variable is visible (1)';
  is $ret, "",                     'my() variable is visible (2)';
}

{
  my $was_in_sub;
  my &foo := -> $arg { $was_in_sub = $arg };
  foo(42);
  is $was_in_sub, 42, 'calling a lexically defined my()-code var worked';
}

{
  is(do{my $a = 3; $a}, 3, 'do{my $a = 3; $a} works');
  is(do{1; my $a = 3; $a}, 3, 'do{1; my $a = 3; $a} works');
}

eval_ok('my $x = my $y = 0; 1', '"my $x = my $y = 0" parses');
