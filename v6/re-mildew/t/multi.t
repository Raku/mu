say "1..2";
my $multi = ::Multi.new;
$multi.variants.push(sub ($arg1 is ref) {
    say $arg1;
});
$multi.variants.push(sub ($arg1,$arg2) {
    say "ok 2";
});
$multi.("ok 1");
$multi.(1,2);
