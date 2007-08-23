use v6-alpha;

module Main {

    say '1..3';

    # my $m = ::Match(
    #    match_str => 'abcdef',
    #    from => 2,
    #    to   => 4,
    #    bool => 1,
    # );
    
    my $m = Match.new; 
    $m.from = 1;
    say "ok ", $m.from, " - accessor";

    $m.from = 2;
    $m.to   = 4;
    $m.bool = 1;
    $m.match_str = 'abcdef';

    if $m.from == 2 {
        say "ok 2 - accessor";
    }
    else {
        say "not ok - got ", $m.from;
    };

    if $m.str eq 'cd' {
        say "ok 3 - match stringify";
    }
    else {
        say "not ok - got ", $m.str;
    }

}