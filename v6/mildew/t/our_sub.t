module Foo {	
    our sub bar($arg) {
        $OUT.print($arg.FETCH);
    }
    module Bla {
    	our sub bar() {
	   $OUT.print("ok 1\n");
        }
    }
}
$OUT.print("1..2\n");
Foo::Bla::bar();
Foo::bar("ok 2\n");
