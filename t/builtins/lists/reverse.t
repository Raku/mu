#!/usr/bin/pugs

use v6;
use Test;

=kwid

tests for "reverse"

=cut

plan 35;

my @a = reverse(1, 2, 3, 4);
my @e = (4, 3, 2, 1);

is(@a, @e, "list was reversed");

my $a = reverse("foo");
is($a, "oof", "string was reversed");

@a = item(reverse("foo"));
is(@a[0], "oof", 'the string was reversed');
@a = list(reverse("foo"));
is(@a[0], "foo", 'the list was reversed');

@a = item(reverse("foo", "bar"));
is(@a[0], "rab oof", 'the stringified array was reversed (stringwise)');
@a = list reverse "foo", "bar";
is(+@a, 2, 'the reversed list has two elements');
is(@a[0], "bar", 'the list was reversed properly');

is(@a[1], "foo", 'the list was reversed properly');

class Foo {
	has @.n;
	method foo () { (1, 2, 3) }
	method bar () {
		return @.n = reverse $?SELF.foo;
	}
}

my @n = Foo.new.bar;

is(+@n, 3, "list context reverse in masak's bug");
is(~@n, "3 2 1", "elements seem reversed");

{    
    my @a = "foo";
    my @b = @a.reverse;
    isa_ok(@b, 'Array');
    my $b = @a.reverse;
    isa_ok($b, 'Str');
    is(@b[0], "foo", 'our list is reversed properly'); 
    is($b, "oof", 'in scalar context it is a string');
    is(@a[0], "foo", "original array left untouched");
    @a.=reverse;
    is(@a[0], "foo", 'in place reversal works');
}

{
    my @a = ("foo", "bar");
    my @b = @a.reverse;
    isa_ok(@b, 'Array');
    my $b = @a.reverse;
    isa_ok($b, 'Str');
    is(@b[0], "bar", 'our array is reversed');
    is(@b[1], "foo", 'our array is reversed');
    
    is($b, "rab oof", 'in scalar context it is a string');
    
    is(@a[0], "foo", "original array left untouched");
    is(@a[1], "bar", "original array left untouched");
    
    @a.=reverse;
    is(@a[0], "bar", 'in place reversal works');
    is(@a[1], "foo", 'in place reversal works');
}

{
    my $a = "foo";
    my @b = $a.reverse;
    isa_ok(@b, 'Array');    
    my $b = $a.reverse;
    isa_ok($b, 'Str');    
    
    is(@b[0], "oof", 'string in the array has been reversed', :todo<bug>);
    is($b, "oof", 'string has been reversed');
    is($a, "foo", "original scalar left untouched");
    $a.=reverse;
    is($a, "oof", 'in place reversal works on strings');
}

{
    my $a = "foo".reverse;
    my @b = "foo".reverse;
    isa_ok($a, 'Str');
    isa_ok(@b, 'Array');
    is($a, "oof", 'string literal reversal works in scalar context');
    is(@b[0], "oof", 'string literal reversal works in list context', :todo<bug>);
}
