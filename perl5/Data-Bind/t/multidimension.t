#!/usr/bin/perl -w
use strict;
use Test::More tests => 5;
use Test::Exception;
use Data::Bind;

sub {
#  my sub foo(*@a; *@b) { }
  my $foo = sub { my (@a,@b); Data::Bind->arg_bind(\@_);
		  map { $a[$_] + $b[$_] } 0..$#a;
  };
  Data::Bind->sub_signature
    ($foo,
     [ { var => '@a', is_slurpy => 1 } ],
     [ { var => '@b', is_slurpy => 1 } ],
    );

  my @array  = <1 2 3>;
  my @array2 = <4 5 6>;

  my @ret = $foo->([\@array], {}, [\@array2], {});
  is_deeply \@ret, [5,7,9];
}->();

sub {
#  my sub foo(*@@args) { }
  my $foo = sub { my @args; Data::Bind->arg_bind(\@_);
		  is($#args, 1);
		  is_deeply($args[0][0], [<a d c>]);
		  is(${$args[0]->{orz}}, 2);
		  is(${$args[1][0]}, 'b');;
  };
  Data::Bind->sub_signature
    ($foo, { var => '@args', is_multidimension => 1 }),

  my @array  = <a b c>;
  my $var    = "d";
  bind_op2(\$array[1] => \$var);

  $foo->([\@array], { orz => \2}, [\'b', \'x'], {});
}->();

## XXX: test for wrong dimension
