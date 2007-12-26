use v6-alpha;


say "1..2";

sub outer1 {
    say "ok 1 - outer";
}

sub inner1 {
    say "ok 2 - inner";
}

outer1();
inner1();

&inner1.set_outer( &outer1 );

