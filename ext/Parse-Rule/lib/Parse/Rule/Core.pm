module Parse::Rule::Core;

class Medium { }

class Match {
    has Medium $.start;
    has Medium $.end;
    
    has Match $.match_num;
    has Match $.match_name;

    submethod BUILD () {
        $.match_num //= [];
        $.match_name //= {};
    }

    sub combine($matches, Medium $pos) {
        my ($start, $end) = $matches 
                                ?? ($matches[0].start, $matches[-1].end)
                                !! ($pos, $pos);
        my $num_tr = [];
        my $name_tr = hash();
        
        for $matches.keys -> $matchno {
            my $m = $matches[$matchno];
            my $n = $m.match_num;
            $num_tr[$_][$matchno]  = $m.match_num[$_]  for $m.match_num.keys;
            $name_tr{$_}[$matchno] = $m.match_name{$_} for $m.match_name.keys;
        }
        
        return Match.new(
            start      => $start,
            end        => $end,
            match_num  => $num_tr,
            match_name => $name_tr,
        );
    }
}

class Result {
    has Medium $.pos;
    has Code $.backtrack;

    has Match $.value;
}

class Parser {
    has Code $.parse;
}

# vim: ft=perl6 :
