say "1..1";
my $array = ::Array.new;
$array.push("not ok 1");
$array.push("ok 1");
$array.push("not ok 1");
my sub is_2($ok) {
    $ok eq "ok 1";
}
$array = grep &is_2,$array;
say $array[0];
