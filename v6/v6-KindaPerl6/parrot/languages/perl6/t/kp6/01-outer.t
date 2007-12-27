use v6-alpha;


say "1..3";

my $x = 42;

sub outer1 {
    my $x = 123;
    say "ok 1 - outer";
}

my $sub = do {
    my $x = 2;
    return sub ($v) {
        print "not " if $v != $x;
        say "ok ",$v," - inner - ", $x;
    }
}

outer1();
$sub(2);

$sub.set_outer( &outer1 );

$sub(3);

