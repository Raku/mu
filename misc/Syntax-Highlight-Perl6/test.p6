class FooClass {
	my $foo-bar;
	$foo-bar;
	
	method foo-method($a, $b, $c) {
	}

	sub foo-sub($a, $b) {
	}
}

FooClass.new.foo-method;

##grammar FooGrammar { }
##package FooPackage { }
##slang FooSlang { }
##module FooModule { }

our $foo;