say "1..3";
my $array := ::Array.new;
$array.push("ok 1");
$array.push("ok 2");
$array.push("ok 3");
for $array -> $ok {
    say $ok;
}
