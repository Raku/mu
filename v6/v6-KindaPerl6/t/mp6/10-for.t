class Main {
    say "1..3";
    my $fields := [6,7,8];
    my $i := 6;
    my $field;
    for @$fields -> $field {
        if $field == $i {
            say "ok";
        } else {
            say "not ok";
        };
        $i := $i+1;
    };
}
