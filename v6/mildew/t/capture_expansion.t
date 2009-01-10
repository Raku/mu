#$OUT.print("1..1\n");
my sub foo($a,$b) {
    $OUT.print($a.FETCH);
    $OUT.print($b.FETCH);
}
knowhow Foo {
    method postcircumfix:<( )>(\$capture) {
        &foo.postcircumfix:<( )>((|$capture));        
    }
}
::Foo.("ok 1\n","ok 2\n");
