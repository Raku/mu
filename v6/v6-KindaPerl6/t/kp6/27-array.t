use v6-alpha;
module Main {
    say "1..6";
    my @array;
    @array[0] = 1;
    @array[1] = 2;
    say "ok "~@array[0];
    say "ok "~@array[1];
    say "ok " ~ ( 1 + @array.elems );
    my @array2;
    @array2[0] = "4";
    @array2[1] = "5";
    @array2.map(sub ($elem) {say "ok "~$elem});
    if @array.join( ',' ) eq '1,2' {
        say "ok 6";
    } else {
        say "not ok";
    }
}
