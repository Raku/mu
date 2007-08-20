use v6-alpha;
module Main {
    say '1..1';

    subset StrOk of Str where { $_ eq 'ok' };

    my StrOk $ok;
    
    $ok = "ok";

    if $ok.does( 'StrOk' ) {
        say "ok 1"
    }
    else {
        say "not ok 1"
    };
}
