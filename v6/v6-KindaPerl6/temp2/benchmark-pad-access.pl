use Benchmark qw(:all) ;

{
    my $i;
    sub outer { do{$i++;$i} for 0 .. 100000 }
}

    sub inner { my $i; do{$i++;$i} for 0 .. 100000 }

cmpthese(1000000, {
    'outer' => outer(),
    'inner' => inner(),
});
