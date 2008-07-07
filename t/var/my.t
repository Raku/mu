use v6;

use Test;

# L<"http://use.perl.org/~autrijus/journal/25337">
# my() declarations scopes lexically to the rest of the block; using $MY::x or
# $::("x") in the block before the actual declaration is erroneous.

plan 7;


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

eval_lives_ok 'my $x = my $y = 0;', '"my $x = my $y = 0" parses';


{
    my $test = "value should still be set for arg, even if there's a later my";
    sub foo (*%p) {
        is(%p<a>, 'b', $test);
        my %p;
    }
    foo(a => 'b');
}
