use v6-alpha;


say "1..3";

my $x = 2;

sub outer1 {
    my $x = 123;
    say "ok 1 - outer";
}

sub inner1($v) {
    print "not " if $v != $x;
    say "ok ",$v," - inner - ", $x;
}

outer1();
inner1(2);

&inner1.set_outer( &outer1 );

inner1(3);

