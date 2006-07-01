use Test::More tests => 1;
use Data::Bind;

Data::Bind->sub_signature
    (\&foo,
     { var => '$bar'},
     { var => '&code'});

use Scalar::Util qw(blessed);

sub foo {
  my ($bar);
  Data::Bind->arg_bind(\@_);
  no warnings 'uninitialized';
  return "$bar:".code();
}

my $out = foo([\'this is a test', \sub {"foo"}]);
is($out, 'this is a test:foo');

