#1/usr/bin/pugs

use v6;
require Test;

plan 15;

my @a; eval '@a = reverse(1, 2, 3, 4)';
my @e = (4, 3, 2, 1);

is(@a, @e, "list was reversed");

my $a; eval '$a = reverse("foo")';
is($a, "oof", "string was reversed");



@a = scalar reverse "foo";
is(@a[0], "oof");
@a = list   reverse "foo";
todo_is(@a[0], "foo");

@a = scalar reverse "foo", "bar";
todo_is(@a[0], "raboof");
@a = list   reverse "foo", "bar";
todo_is(@a[0], "bar");
todo_is(@a[1], "foo");

my @b;
my $b;

@a = "foo";
@b = @a.reverse;
$b = @a.reverse;
todo_is(@b[0], "foo");
todo_is($b[0], "foo");
is(@a[0], "foo", "original array left untouched");
@a.=reverse;
todo_is(@a[0], "foo");

@a = ("foo", "bar");
@b = @a.reverse;
$b = @a.reverse;
todo_is(@b[0], "bar");
todo_is($b[0], "bar");
todo_is(@b[1], "foo");
todo_is($b[1], "foo");
is(@a[0], "foo", "original array left untouched");
is(@a[1], "bar", "original array left untouched");
@a.=reverse;
todo_is(@a[0], "bar");
todo_is(@a[1], "foo");

$a = "foo";
@b = $a.reverse;
$b = $a.reverse;
is(@b[0], "oof");
is($b,    "oof");
is($a, "foo", "original scalar left untouched");
$a.=reverse;
todo_is($a,    "oof");
