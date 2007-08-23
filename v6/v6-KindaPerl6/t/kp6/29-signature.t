class Main {
    my sub ab ($a,$b) {
        if ($a == $b) {
            say "ok";
        } else {
            say "not ok # "~$a~" != "~$b;;
        }
    };

    say '1..1';
    say 'ok 1';

    say '# Signature: ', &ab.signature;
}
