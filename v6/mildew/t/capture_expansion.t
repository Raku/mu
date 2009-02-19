say "1..2";
my sub foo($a,$b) {
    say $a;
    say $b;
}
knowhow Foo {
    method postcircumfix:<( )>(\$capture) {
        &foo.postcircumfix:<( )>((|$capture));        
    }
}
::Foo.("ok 1\n","ok 2\n");
