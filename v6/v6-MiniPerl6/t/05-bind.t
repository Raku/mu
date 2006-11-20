use v6-alpha;

class Main {

do {
    say '1..6';

    my $x := 5;
    if $x != 5 {
        print 'not '
    };
    say 'ok';
};

do {
    my $x := 6;
    my $y := 7;
    my $x1;
    my $y1;
    [ $x1, $y1 ] := [ $x, $y ];
    if $x1 != 6 {
        print 'not '
    };
    say 'ok';
    if $y1 != 7 {
        print 'not '
    };
    say 'ok';
};

do {
    my $x := 9;
    my $y := 10;
    my $x1;
    my $y1;
    my $z1;
    [ $x1, [ $y1, $z1 ] ] := [ $x, [$y, 11] ];
    if $x1 != 9 {
        print 'not '
    };
    say 'ok';
    if $y1 != 10 {
        print 'not '
    };
    say 'ok';
    if $z1 != 11 {
        print 'not '
    };
    say 'ok';
};

}
