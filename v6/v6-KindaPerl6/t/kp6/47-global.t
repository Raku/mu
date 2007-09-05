module Main {
    $Main::foo = 'ok 1';
    
    ok();
    
    sub ok {
        say $Main::foo;
    };
}