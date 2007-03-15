#!/usr/bin/perl -w
# Translated t/scalar.t
use strict;
use Test::More tests => 7;
use Test::Exception;
use Data::Bind;

use Scalar::Util 'refaddr';

sub id_eq {
    my ($x, $y) = @_;
    return refaddr($x) == refaddr($y);
}

# L<S03/"Binding" /replaces the container itself\.  For instance:/>
# Basic scalar binding tests
sub {
  my $x = bless { str => 'Just Another' }, 'Str';
  is($x->{str}, 'Just Another', 'normal assignment works');

  my $y;
  bind_op('$y', \$x);
  is($y->{str}, 'Just Another', 'y is now bound to x');
  is(ref $y, 'Str', 'y is still blessed');

}->();

# Binding subroutine parameters
# XXX! When executed in interactive Pugs, the following test works!
{
  my $a;
  my $b = sub { my $arg; Data::Bind->arg_bind(\@_);
		bind_op2(\$a => \$arg) };
  Data::Bind->sub_signature
    ($b, { var => '$arg' });

  my $val = bless { val => 42 }, 'Int';

  $b->([\$val]);
  is $a->{val}, 42, "bound readonly sub param was bound correctly (1)";
  is(ref $a, 'Int', 'a is still blessed');

}

{
  my $a;
#  my $b = sub($arg is rw) { $a := $arg };
  my $b = sub { my $arg; Data::Bind->arg_bind(\@_); bind_op2(\$a => \$arg) };
  Data::Bind->sub_signature
    ($b, { var => '$arg', is_rw => 1 });
  my $val = bless { val => 42 }, 'Int';

  $b->([\$val]);
  is $a->{val}, 42, "bound rw sub param was bound correctly (1)";
  is(ref $a, 'Int', 'a is still blessed');

}

