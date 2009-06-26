=begin Pod
=end Pod
class FooClass is B {
	my $foo-bar;
	$foo-bar;
	
	has $name;
	
	method sum($a, $b) {
		{ my $foo;}
		self;
		return $a + $b;
	}

	sub foo-sub($a, $b) {
		sub inner { }
	}
	
	grammar FooInside {
		token t { \d }
	}
}

sub _foo() {
}
while (0) { }
if (1) { my $foo; } elsif (1) { } else { }
unless (1) { };
FooClass.new.foo-method;
our $foo = 1.0;