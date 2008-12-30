module Foo {	
    our sub bar() {
        $OUT.print("ok 2\n");
    }
    module Bla {
    	our sub bar() {
	   $OUT.print("ok 1\n");
        }
    }
}
$OUT.print("1..2\n");
Foo::Bla::bar();
Foo::bar();
