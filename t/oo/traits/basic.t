use v6-alpha;

use Test;

plan 9;

=pod

Basic traits tests, see L<S12/Traits>.

=cut

# L<S12/Traits>
# Basic definition
my $was_in_any_sub   = 0;
my $was_in_class_sub = 0;
role cool {
  has $.is_cool = 42;

  multi sub trait_auxiliary:<is>(cool $trait, Any $container:) {
    $was_in_any_sub++;
    $container does cool;
  }

  multi sub trait_auxiliary:<is>(cool $trait, Class $container:) {
    $was_in_class_sub++;
    $container does cool;
  }
}
ok(::cool.HOW, "role definition worked");

eval_ok 'my $a is cool; 1', 'mixing in our role into a scalar via "is" worked';
is $was_in_any_sub, 1, 'trait_auxiliary:is was called on container', :todo<feature>;
eval_is '$a.is_cool', 'our var "inherited" an attribute', :todo<feature>;

my $b;
class B is cool;
ok(::B.HOW, 'mixing in our role into a class via "is" worked');
is $was_in_class_sub, 1, 'trait_auxiliary:is was called on class', :todo<feature>;
$b = B.new;
ok($b, 'creating an instance worked');
is($b.is_cool,    42,  'our class "inherited" an attribute', :todo<feature>);

is(!eval(' %!P = 1; 1'),
   undef, 'calling a trait outside of a class should be a syntax error', :todo<bug>);
