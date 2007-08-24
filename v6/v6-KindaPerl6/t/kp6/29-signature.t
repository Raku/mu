class Main {
    my sub ab ($a,$b) {
        if ($a == $b) {
            say "ok";
        } else {
            say "not ok # "~$a~" != "~$b;;
        }
    };

    say '1..2';
    say 'ok 1';

    say '# Signature: ', &ab.signature;
    if (&ab.signature).arity == 2 {
        say "ok 2 arity";
    }
    else {
        say "not ok 2"
    }
}
