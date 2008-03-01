class Main {
    say "1..1";
    my $hash := {a=>2,b=>3};
    if $hash{'a'} == '2' {
        say "ok";
    } else {
        say "not ok";
    };
}
