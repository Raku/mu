use v6-alpha;


module Main {

    say '1..2';
    
    my $m = Capture.new; 
    $m.invocant = 123;
    if $m.invocant == 123 {
        say "ok 1 - invocant";
    }
    else {
        say "not ok - got ", $m.invocant;
    };

    my $c = \(4:5,6,7);
    if $c.invocant == 4 {
        say "ok 2 - invocant 2";
    } else {
        say "not ok - got ", $c.invocant;
    }
}
