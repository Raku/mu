#!/usr/bin/perl -w
use strict;
use Test::More tests => 48;
use Test::Exception;
use Data::Bind;
# L<S03/"Binding">

# Binding of array elements.
# See thread "Binding of array elements" on p6l started by Ingo Blechschmidt:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/22915">

sub {
  my @array  = <a b c>;
  my $var    = "d";

  eval { bind_op2(\$array[1] => \$var) };

  is $array[1], "d", "basic binding of an array element (1)";

  $var = "e";
  is $array[1], "e", "basic binding of an array element (2)";

  $array[1] = "f";
  is $var,      "f", "basic binding of an array element (3)";
}->();

sub {
  my @array  = <a b c>;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);

  $var       = "e";
  is $array[1], "e",  "binding of array elements works with .delete (1)";

  delete $array[1];
  # $var unchanged, but assigning to $var doesn't modify @array any
  # longer; similarily, changing @array[1] doesn't modify $var now
  is $var,    "e",    "binding of array elements works with .delete (2)";
  is_deeply \@array, ['a',undef,'c'], "binding of array elements works with .delete (3)";

  $var      = "f";
  $array[1] = "g";
  is $var,      "f",  "binding of array elements works with .delete (4)";
  is $array[1], "g",  "binding of array elements works with .delete (5)";
}->();

sub {
  my @array  = <a b c>;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);

  $var       = "e";
  is $array[1], "e", "binding of array elements works with resetting the array (1)";

  @array = ();
  # $var unchanged, but assigning to $var doesn't modify @array any
  # longer; similarily, changing @array[1] doesn't modify $var now
  is $var,    "e",   "binding of array elements works with resetting the array (2)";
  is_deeply \@array, [],    "binding of array elements works with resetting the array (3)";

  $var      = "f";
  $array[1] = "g";
  is $var,      "f", "binding of array elements works with resetting the array (4)";
  is $array[1], "g", "binding of array elements works with resetting the array (5)";
}->();

sub {
  my @array  = <a b c>;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);
  $var       = "e";
  is $array[1], "e",   "binding of array elements works with rebinding the array (1)";

  my @other_array = <x y z>;
  bind_op('@array' => \@other_array);
  # $var unchanged, but assigning to $var doesn't modify @array any
  # longer; similarily, changing @array[1] doesn't modify $var now
  is $var,    "e",     "binding of array elements works with rebinding the array (2)";
  is_deeply \@array, [qw(x y z)], "binding of array elements works with rebinding the array (3)";

  $var      = "f";
  $array[1] = "g";
  is $var,      "f",   "binding of array elements works with rebinding the array (4)";
  is $array[1], "g",   "binding of array elements works with rebinding the array (5)";
}->();

sub {
#  my sub foo (@arr) { @arr[1] = "new_value" }
  my $foo = sub { my @arr; Data::Bind->arg_bind(\@_);
            $arr[1] = "new_value";
  };
  Data::Bind->sub_signature
    ($foo, { var => '@arr' }),

  my @array  = <a b c>;
  my $var    = "d";
  bind_op2(\$array[1], \$var);

  is($array[1], $var, "bind_op2");

  lives_ok { $foo->([\@array]) };
  is $var,    "new_value",     "passing an array to a sub expecting an array behaves correctly (1)";
  is_deeply \@array, [<a new_value c>], "passing an array to a sub expecting an array behaves correctly (2)";
}->();

sub {
#  my sub foo (Array $arr) { $arr[1] = "new_value" }
  my $foo = sub { my $arr; Data::Bind->arg_bind(\@_);
		  $arr->[1] = "new_value";
  };
  Data::Bind->sub_signature
    ($foo, { var => '$arr', isa => 'Array' }),

  my @array  = <a b c>;
  my $var    = "d";
  bind_op2(\$array[1] => \$var);
  $foo->([\\@array]);
  is $var,    "new_value",     "passing an array to a sub expecting an arrayref behaves correctly (1)";
  is_deeply \@array, [<a new_value c>], "passing an array to a sub expecting an arrayref behaves correctly (2)";
}->();

sub {
#  my sub foo (*@args) { @args[1] = "new_value" }
  my $foo =sub { my @args; Data::Bind->arg_bind(\@_);
            $args[1] = "new_value";
  };
  Data::Bind->sub_signature
    ($foo, { var => '@args', is_slurpy => 1 }),

  my @array  = <a b c>;
  my $var    = "d";
  bind_op2(\$array[1], \$var);

  $foo->([\@array]);
  is $var,    "new_value",     "passing an array to a slurpying sub behaves correctly (1)";
  is_deeply \@array, [<a new_value c>], "passing an array to a slurpying sub behaves correctly (2)";
}->();

sub {
#  my sub foo (*@args) { push @args, "new_value" }
  my $foo = sub { my @args; Data::Bind->arg_bind(\@_);
            push @args, "new_value";
  };
  Data::Bind->sub_signature
    ($foo, { var => '@args', is_slurpy => 1 }),

  my @array  = <a b c>;
  my $var    = "d";
  bind_op2(\$array[1] => \$var);

  $foo->([\@array]);
  is $var,    "d",     "passing an array to a slurpying sub behaves correctly (3)";
  is_deeply \@array, [<a d c>], "passing an array to a slurpying sub behaves correctly (4)";
}->();

# Binding of not yet existing elements should autovivify
{
  my @array;
  my $var    = "d";

  lives_ok { bind_op2(\$array[1] => \$var) }
                     "binding of not yet existing elements should autovivify (1)";
  is $array[1], "d", "binding of not yet existing elements should autovivify (2)";

  $var = "e";
  is $array[1], "e", "binding of not yet existing elements should autovivify (3)";
  is $var,      "e", "binding of not yet existing elements should autovivify (4)";
}

# Binding with .splice
{
  my @array  = <a b c>;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);
  $var       = "e";
  is $array[1], "e",  "binding of array elements works with splice (1)";

  splice @array, 1, 1, ();
  # $var unchanged, but assigning to $var doesn't modify @array any
  # longer; similarily, changing @array[1] doesn't modify $var now
  is $var,    "e",    "binding of array elements works with splice (2)";
  is_deeply \@array, ['a', 'c'], "binding of array elements works with splice (3)";

  $var      = "f";
  $array[1] = "g";
  is $var,      "f",  "binding of array elements works with splice (4)";
  is $array[1], "g",  "binding of array elements works with splice (5)";
}

# Assignment (not binding) creates new containers
sub {
  my @array  = <a b c>;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);
  $var       = "e";
  is $array[1], "e",       "array assignment creates new containers (1)";

  my @new_array = @array;
  $var          = "f";
  # @array[$idx] and $var are now "f", but @new_array is unchanged.
  is $var,        "f",     "array assignment creates new containers (2)";
  is_deeply \@array,     [<a f c>], "array assignment creates new containers (3)";
  is_deeply \@new_array, [<a e c>], "array assignment creates new containers (4)";
}->();

# Binding does not create new containers
sub {
  my @array  = <a b c>;
  my @new_array;
  my $var    = "d";

  bind_op2(\$array[1] => \$var);

  $var       = "e";
  is $array[1], "e",       "array binding does not create new containers (1)";

  bind_op('@new_array' => \@array);
  $var          = "f";
  # @array[$idx] and $var are now "f", but @new_array is unchanged.
  is $var,        "f",     "array binding does not create new containers (2)";
  is_deeply \@array,     [qw(a f c)], "array binding does not create new containers (3)";
  is_deeply \@new_array, [qw(a f c)], "array binding does not create new containers (4)";
}->();

# Binding @array := $arrayref.
# See
# http://colabti.de/irclogger/irclogger_log/perl6?date=2005-11-06,Sun&sel=388#l564
# and consider the magic behind parameter binding (which is really normal
# binding).
sub {
  my $arrayref  = [<a b c>];
  my @array;
  # my @array    := $arrayref;

  bind_op('@array' => $arrayref);
  is +@array, 3,          'binding @array := $arrayref works (1)';

  $array[1] = "B";
  is_deeply $arrayref,  [<a B c>], 'binding @array := $arrayref works (2)';
  is_deeply \@array,    [<a B c>], 'binding @array := $arrayref works (3)';
}->();
