#1/usr/bin/pugs

use v6;
require Test;

plan 2;

my @a; eval '@a = reverse(1, 2, 3, 4)';
my @e = (4, 3, 2, 1);

todo_is(@a, @e, "list was reversed");

my $a; eval '$a = reverse("foo")';
todo_is($a, "oof", "string was reversed");

