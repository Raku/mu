#!/usr/bin/perl
use Test::More tests => 26;

=pod

=head1 List parameter test

These tests are the testing for "List paameters" section of Synopsis 06

L<<S06/"List parameters" /Slurpy parameters follow any required or optional parameters. They are marked by a * before the parameter:/>>

You might also be interested in the thread Calling positionals by name in
presence of a slurpy hash" on p6l started by Ingo
Blechschmidt L<"http://www.nntp.perl.org/group/perl.perl6.language/22883">

=cut

use Test::Exception;
use Data::Bind;
{
 no warnings 'uninitialized';
 my @sig =
    ({ var => '$n' },
     { var => '%h', is_slurpy => 1, named_only => 1 },
     { var => '@a', is_slurpy => 1 });
# Positional with slurpy *%h and slurpy *@a
#my sub foo($n, *%h, *@a) { }
sub foo { my ($n, %h, @a); Data::Bind->arg_bind(\@_) };
Data::Bind->sub_signature(\&foo, @sig);

#my sub foo1($n, *%h, *@a) { $n }
sub foo1 { my ($n, %h, @a); Data::Bind->arg_bind(\@_);
	   $n }
Data::Bind->sub_signature(\&foo1, @sig);
#my sub foo2($n, *%h, *@a) { %h<x> + %h<y> + %h<n> }
sub foo2 { my ($n, %h, @a); Data::Bind->arg_bind(\@_);
	   $h{x} + $h{y} + $h{n} }
Data::Bind->sub_signature(\&foo2, @sig);

#my sub foo3($n, *%h, *@a) { @a.sum }
use List::Util 'sum';
sub foo3 { my ($n, %h, @a); Data::Bind->arg_bind(\@_);
	   sum @a }
Data::Bind->sub_signature(\&foo3, @sig);

## all pairs will be slurped into hash, except the key which has the same name
## as positional parameter
diag('Testing with positional arguments');
lives_ok { foo [\1, \4000], {x => \20, y => \300} }
  'Testing: `sub foo($n, *%h, *@a){ }; foo 1, x => 20, y => 300, 4000`';
is ((foo1 [\1, \4000], {x => \20, y => \300}), 1,
  'Testing the value for positional');
is ((foo2 [\1, \4000], {x => \20, y => \300}), 320,
  'Testing the value for slurpy *%h');
is ((foo3 [\1, \4000], {x => \20, y => \300}), 4000,
  'Testing the value for slurpy *@a');

TODO: {
local $TODO = 'not yet';
dies_ok { foo [\1, \4000], {n => \20, y => \300} }
  'Testing: `sub foo($n, *%h, *@a){ }; foo 1, n => 20, y => 300, 4000`';
}

## We *can* pass positional arguments as a 'named' pair with slurpy *%h.
## Only *remaining* pairs are slurped into the *%h
# Note: with slurpy *@a, you can pass positional params, But will be slurped into *@a
diag('Testing without positional arguments');
lives_ok { foo [\4000], {n => \20, y => \300} }
  'Testing: `sub foo($n, *%h, *@a){ }; foo n => 20, y => 300, 4000`';
is ((foo1 [\4000], {n => \20, y => \300}), 20,
  'Testing the value for positional');
is ((foo2 [\4000], {n => \20, y => \300}), 300,
  'Testing the value for slurpy *%h');
is ((foo3 [\4000], {n => \20, y => \300}), 4000,
  'Testing the value for slurpy *@a');
}

sub {
local $TODO = 'unbound arg should die';
#my sub foo ($n, *%h) { };
my $foo = sub { my ($n, %h); Data::Bind->arg_bind(\@_) };
Data::Bind->sub_signature($foo, { var => '$n' },
 { var => '%h', is_slurpy => 1, named_only => 1 });
## NOTE: *NOT* sub foo ($n, *%h, *@a)
dies_ok { $foo->([\1], {n => \20, y => \300}) }
  'Testing: `sub foo($n, *%h) { }; foo 1, n => 20, y => 300`';

#my sub foo ($n, *%h) { };
## same $foo
## NOTE: *NOT* sub foo ($n, *%h, *@a)
dies_ok { $foo->([\1, \4000], {x => \20, y => \300}) }
  'Testing: `sub foo($n, *%h) { }; foo 1, x => 20, y => 300, 4000`';
}->();

# Named with slurpy *%h and slurpy *@a
# named arguments aren't required in tests below
{
#my sub foo(:$n, *%h, *@a) { };
#my sub foo1(:$n, *%h, *@a) { $n };
#my sub foo2(:$n, *%h, *@a) { %h<x> + %h<y> + %h<n> };
#my sub foo3(:$n, *%h, *@a) { return @a.sum };
my @sig = 
    ({ var => '$n', named_only => 1 },
     { var => '%h', is_slurpy => 1, named_only => 1 },
     { var => '@a', is_slurpy => 1 });
Data::Bind->sub_signature(\&foo, @sig);
Data::Bind->sub_signature(\&foo1, @sig);
Data::Bind->sub_signature(\&foo2, @sig);
Data::Bind->sub_signature(\&foo3, @sig);

diag("Testing with named arguments (named param isn't required)");
lives_ok { foo [\1, \4000], {x => \20, y => \300} }
  'Testing: `sub foo(:$n, *%h, *@a){ }; foo 1, x => 20, y => 300, 4000`';
is((foo1 [\1, \4000], {x => \20, y => \300}), undef,
  'Testing value for named argument');
is((foo2 [\1, \4000], {x => \20, y => \300}), 320,
  'Testing value for slurpy *%h');
is((foo3 [\1, \4000], {x => \20, y => \300}), 4001,
  'Testing the value for slurpy *@a');


### named parameter pair will always have a higher "priority" while passing
### so %h<n> will always be undef
lives_ok { foo1 [\1, \4000], {n => \20, y => \300} }
  'Testing: `sub foo(:$n, *%h, *@a){ }; foo 1, n => 20, y => 300, 4000`';
is((foo1 [\1, \4000], {n => \20, y => \300}), 20,
  'Testing the named argument');
is((foo2 [\1, \4000], {n => \20, y => \300}), 300,
  'Testing value for slurpy *%h');
is((foo3 [\1, \4000], {n => \20, y => \300}), 4001,
  'Testing the value for slurpy *@a');
}

# named with slurpy *%h and slurpy *@a
## Named arguments **ARE** required in tests below

#### ++ version
{
#my sub foo(:$n!, *%h, *@a){ };
Data::Bind->sub_signature(\&foo,
 { var => '$n', named_only => 1, required => 1 },
 { var => '%h', is_slurpy => 1, named_only => 1 },
 { var => '@a', is_slurpy => 1 });

diag('Testing with named arguments (named param is required) (++ version)');
lives_ok { foo [\1, \4000], {n => \20, y => \300} }
  'Testing: `my sub foo(+:$n, *%h, *@a){ }; foo 1, n => 20, y => 300, 4000 }`';
dies_ok { foo [\1, \4000], {x => \20, y => \300} };
}

#### "trait" version
{
#my sub foo(:$n is required, *%h, *@a) { };
diag('Testing with named arguments (named param is required) (trait version)');
lives_ok { foo [\1, \4000], {n => \20, y => \300} }
  'Testing: `my sub foo(:$n is required, *%h, *@a){ }; foo 1, n => 20, y => 300, 4000 }`';
dies_ok { foo [\1, \4000], {x => \20, y => \300} }
  'Testing: `my sub foo(:$n is required, *%h, *@a){ }; foo 1, x => 20, y => 300, 4000 }`';
}

##### Now slurpy scalar tests here.
=kwid

=head1 List parameter test

These tests are the testing for "List paameters" section of Synopsis 06

L<<S06/"List parameters" /Slurpy scalar parameters capture what would otherwise be the first elements of the variadic array:/>>

=cut

#sub first(*$f, *$s, *@r){ return $f };
my @sig =
    ({ var => '$f', is_slurpy => 1 },
     { var => '$s', is_slurpy => 1 },
     { var => '@r', is_slurpy => 1 });
sub first { my ($f, $s, @r); Data::Bind->arg_bind(\@_);
	    return $f };
Data::Bind->sub_signature(\&first, @sig);

#sub second(*$f, *$s, *@r){ return $s };
sub second { my ($f, $s, @r); Data::Bind->arg_bind(\@_);
	    return $s };
Data::Bind->sub_signature(\&second, @sig);

#sub rest(*$f, *$s, *@r){ return @r.sum };
sub rest { my ($f, $s, @r); Data::Bind->arg_bind(\@_);
	    return sum(@r) };
Data::Bind->sub_signature(\&rest, @sig);

diag 'Testing with slurpy scalar';
is first([map { \$_} (1, 2, 3, 4, 5)]), 1,
  'Testing the first slurpy scalar...';
is second([map { \$_} (1, 2, 3, 4, 5)]), 2,
  'Testing the second slurpy scalar...';
is rest([map { \$_} (1, 2, 3, 4, 5)]), 12,
  'Testing the rest slurpy *@r';

