my sub bar {
    my $foo = "ok 1\n";
    say MY::<$foo>;
    say CALLER::<$foo>;
}
my $foo = "ok 2\n";
say "1..2";
bar();
