use v6-alpha;

class Match {
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;
    has $.array;
    has $.hash;
    
    method str {
           self.bool 
        ?? substr( self.match_str, self.from, self.to - self.from )
        !! undef;
    }
}
module Main {

    # XXX - Prelude loading should be automatic
    use KindaPerl6::Runtime::Perl6::Prelude;

    say '1..2';

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