use v6-alpha;
module Main {
    say "1..3";
    my @array;
    @array[0] = 1;
    @array[1] = 2;
    say "ok "~@array[0];
    say "ok "~@array[1];
    say "ok " ~ ( 1 + @array.elems );
    my @array2;
    @array2[0] = "dummy";
    @array2.map(sub {say "ok 4"});
    say "# ", @array.join( ',' );
}
