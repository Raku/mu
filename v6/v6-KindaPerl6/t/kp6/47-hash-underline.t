module Main {

    say '1..1';

    my sub x { 
        say '# ', %_;
    }
    
    x( a => 123 );

    say 'ok 1';
}