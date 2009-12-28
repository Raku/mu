module Foo {	
    our sub bar($arg) {
        say $arg;
    }
    module Bla {
    	our sub bar() {
	   say "ok 1";
        }
    }
}
say "1..2";
Foo::Bla::bar();
Foo::bar("ok 2");
