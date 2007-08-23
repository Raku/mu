use v6-alpha;

module Main {

say '1..1';

my $counter = 0;
$counter = $counter + 1;
if ($counter == 1) {
    say "ok ", $counter;
} else {
    say "not ok";
}

}
