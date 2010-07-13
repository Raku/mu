sub foo {
    my $foo = 0;
    loop {
        say "ok";
        if $foo {return}
        $foo = 1;
    }
}
say "1..2";
foo();
