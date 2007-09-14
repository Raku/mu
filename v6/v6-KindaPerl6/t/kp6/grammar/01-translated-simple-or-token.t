grammar MyGrammar {
    method opt_ws ( $str, $pos ) {
        if (!(defined($str))) {
            $str = $_;
        };
        my $MATCH;
        $MATCH = Match.new();
        $MATCH.match_str = $str;
        $MATCH.from = $pos;
        $MATCH.to = $pos;
        $MATCH.bool = 1;
        $MATCH.bool = do {
            my $pos1 := $MATCH.to;
            #warn "Parsing... (" ~ $str ~ ") len: " ~ length($str);
            do {
                (
                 do {
                     #warn "Parsing rule ' '...";
                     if (length($str) < 1) {
                         #warn "Smaller than 1...";
                         (0)
                     } else {
                         #warn "Not smaller than 1...";
                         if (' ' eq substr($str, $MATCH.to, 1)) {
                             #warn "Matched ' '...";
                             (1 + ($MATCH.to = 1 + $MATCH.to ))
                         } else {
                             #warn "Not Matched ' '";
                             (0)
                         }
                     }
                 }
                )
            } || do {
                $MATCH.to = $pos1;
                #warn "Getting back position, returning 1...";
                (1)
            }
        };
        return $MATCH
    };
};
module Main {
    say '1..1';
    $_ = '';
    if MyGrammar.opt_ws() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};
