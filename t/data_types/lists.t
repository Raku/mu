#!/usr/bin/pugs

use v6;
use Test;

# XXX -- Lists are not real datatypes, but I haven't found a better location
# for this test. See
# http://www.nntp.perl.org/group/perl.perl6.language/22924.

plan 14;

# Indexing lists
{
  my $foo = 42;

  try { ($foo, "does_not_matter")[0] = 23 };
  is $foo, 23, "assigning a list element changed the original variable";
}

{
  my $foo = 42;

  is ($foo, "does_not_matter")[-2], 42,
    "indexing lists by a negative index works correctly";
}

# List construction does not create new containers
{
  my $foo = 42;

  ok ($foo, "does_not_matter")[0] =:= $foo,
    "list construction should not create new containers";
}

{
  my $foo = 42;
  ok ($foo, "does_not_matter", 17)[0,1][0] =:= $foo,
    "list construction and list slicing should not create new containers";
}

# Lists as lvalues
{
  my $foo = 42;
  my $bar = 43;

  ($foo, $bar) = (23, 24);
  ok $foo == 23 && $bar eq 24,
    "using lists as lvalues works";
}

{
  my $foo = 42;

  lives_ok { ($foo, undef) = (23, 24) },
    "using lists with embedded undefs as lvalues works (1)";
  ok $foo == 23,
    "using lists with embedded undefs as lvalues works (2)";
}

# List slices as lvalues
{
  my $foo = 42;
  my $bar = 43;

  try { ($foo, 42, $bar, 19)[0, 2] = (23, 24) };
  ok $foo == 23 && $bar == 24,
    "using list slices as lvalues works (1)";

  dies_ok { ($foo, 42, $bar, 19)[1, 3] = (23, 24) },
    "using list slices as lvalues works (2)";
}

# Lists as lvalues used to swap variables
{
  my $foo = 42;
  my $bar = 23;

  ($foo, $bar) = ($bar, $foo);
  ok $foo == 23 && $bar == 42,
    "using lists as lvalues to swap two variables works";
}

{
  my $foo = 1;
  my $bar = 2;
  my $baz = 3;

  ($foo, $bar, $baz) = ($baz, $foo, $bar);
  ok $foo == 3 && $bar == 1 && $baz == 2,
    "using lists as lvalues to swap three variables works";
}

# Lists as lvalues to swap, this time we use binding instead of assignment
{
  my $foo = 42;
  my $bar = 23;

  ($foo, $bar) := ($bar, $foo);
  ok $foo == 23 && $bar == 42,
    "using lists as lvalues in a binding operation to swap two variables works";

  $foo = "some_new_value";
  is $foo, "some_new_value",
    "the vars didn't lose the readwrite-ness";
}

{
  my $foo = 1;
  my $bar = 2;
  my $baz = 3;

  ($foo, $bar, $baz) := ($baz, $foo, $bar);
  ok $foo == 3 && $bar == 1 && $baz == 2,
    "using lists as lvalues in a binding operation to swap three variables works";
}

=begin more-discussion-needed

# \(...) should create a list of references, as in Perl 5.
# See the long thread "\(...)" on p6l started by Ingo Blechschmidt.
{
  my @array_of_refs = \(1,2,3);

  is +@array_of_refs,           3, '\(...) creates a list of references (1)';
  is try{${@array_of_refs[1]}}, 2, '\(...) creates a list of references (2)';
}

{
  my $foo = 42;
  my @array_of_refs = \(1,$foo,3);

  is +@array_of_refs,  3, '\(...) creates a list of references (3)';
  try { ${ @array_of_refs[1] }++ };
  is $foo,            43, '\(...) creates a list of references (4)';
}

=begin more-discussion-needed

See http://www.nntp.perl.org/group/perl.perl6.language/23085:
>     \(@array);  # List of refs to @array's elements, i.e. same as
>     map { \$_ } @array;
>     # Weird (violating the "parens are only for grouping" rule), but
>     # consistent with Perl 5.
> 
> Correct?

{
  my @array         = (1,2,3);
  my @array_of_refs = \(@array);

  is +@array_of_refs,      3, '\(...) creates a list of references (5)';
  is ${@array_of_refs[1]}, 2, '\(...) creates a list of references (6)';
}

{
  my @array         = (1,2,3);
  my @array_of_refs = \(@array,);

  is +@array_of_refs,      3, '\(...) creates a list of references (7)';
  is ${@array_of_refs[1]}, 2, '\(...) creates a list of references (8)';
}

{
  my @array    = (1,2,3);
  my $arrayref = \@array;

  is +$arrayref,    3, '\@array creates an arrayref (1)';
  is +$arrayref[1], 2, '\@array creates an arrayref (2)';
}
