my @examples  = <fp hanoi life mandel quicksort sendmoremoney shuffle>;

my @outputs   = <fp hanoi             quicksort>;

say "1.." ~ (@examples + @outputs);

my $c = 0;

for (@examples) {
    ++$c;
    say "$c ok # skip: Try to compile $_\.p6";
    # when (@outputs) {
    if ($_ eq any(@outputs)) {
        ++$c;
        say "$c ok # skip: Try to run $_\.p6 and compare to output/$_";
    }
}
