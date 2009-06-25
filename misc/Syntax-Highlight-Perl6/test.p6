=begin Pod
=end Pod
class FooClass {
	my $foo-bar;
	$foo-bar;
	
	method foo-method($a, $b, $c) {
	}

	sub foo-sub($a, $b) {
	}
}

while (0) { }
if (1) { } elsif (1) { } else { }
unless (1) { };
FooClass.new.foo-method;

##grammar FooGrammar { }
##package FooPackage { }
##slang FooSlang { }
##module FooModule { }

our $foo = 1.0;