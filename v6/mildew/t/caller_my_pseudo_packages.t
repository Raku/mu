my sub bar {
    my $foo = "ok 1";
    say MY::<$foo>;
    say CALLER::<$foo>;
}
{
my $foo = "ok 2";
say "1..2";
bar();
}
