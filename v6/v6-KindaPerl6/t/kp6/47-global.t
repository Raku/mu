module Main {

    say '1..1';

    $Main::foo = 'ok 1';
    
    # ok();   XXX - bug - forward definitions don't work yet
    
    sub ok {
        say $Main::foo;
    };

    ok();
    
}