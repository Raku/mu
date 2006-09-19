use Test::More tests => 3;
use Data::Bind;

sub foo {
  my ($bar);
  Data::Bind->arg_bind(\@_);
  return "bar is $bar";
}

my $x = bless { id => 9, name => 'fnord' }, 'Xyz';
is("x is $x", 'x is 9: fnord');

Data::Bind->sub_signature
    (\&foo, { var => '$bar', is_rw => 1});
is(foo([\$x]), 'bar is 9: fnord');

Data::Bind->sub_signature
    (\&foo, { var => '$bar'} );
is(foo([\$x]), 'bar is 9: fnord');

package Xyz;

use overload '""' => \&as_string, fallback => 1;

use Scalar::Util 'reftype';

sub as_string {
    my $self = shift;
    warn reftype($self);
    "$self->{id}: $self->{name}";
}
