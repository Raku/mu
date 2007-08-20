use v6-alpha;
module Main {
    say '1..4';

    subset StrOk of Str where { $_ eq 'ok' };

    my StrOk $ok;
    
    $ok = "ok";

    if $ok.does( 'StrOk' ) {
        say "ok 4"
    }
    else {
        say "not ok 4"
    };
}
