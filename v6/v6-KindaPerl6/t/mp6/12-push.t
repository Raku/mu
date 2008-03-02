class Main {
    say "1..1";
    my $array := [];
    push @$array,7;
    push @$array,3;

    if $array[1] == 3 {
        say "ok";
    } else {
        say "not ok";
    };
}
