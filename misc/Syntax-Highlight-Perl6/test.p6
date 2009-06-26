=begin Pod
=end Pod
class FooClass {
	my $foo-bar;
	$foo-bar;
	
	method sum($a, $b) {
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

##grammar FooGrammar { }
##package FooPackage { }
##slang FooSlang { }
##module FooModule { }

our $foo = 1.0;