use v6-alpha;

use Test;

plan 3;

my %e = ("foo", "bar", "blah", "blah");

my %foo = (
        "foo", "bar",
        "blah", "blah",
);

is(+%foo, +%e, "oh boy, it evaluates correctly, too");
is(%foo<foo>, %e<foo>, "...");
is(%foo<blah>, %e<blah>, "...");

