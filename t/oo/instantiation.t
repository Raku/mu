use v6-alpha;

use Test;

plan 2;

class Foo {
	method test1 {
		my $var = Bar.new;
		return $var.value;
	}
	method test2 {
		my Bar $var .= new;
		return $var.value;
	}
}

class Bar {
	method value { "1" }
}

my Foo $baz .= new;
lives_ok { $baz.test1; $baz.test1 },
"Multiple method calls can be made in the same instance, to the same method. (1)";
my Foo $bar .= new;
lives_ok { $bar.test2; $bar.test2 },
"Multiple method calls can be made in the same instance, to the same method. (2)";
