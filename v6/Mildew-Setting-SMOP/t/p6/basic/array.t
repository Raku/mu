say "1..6";
my $array = ::Array.new;
$array[1] = "ok 1";
$array.push("ok 2");
$array.unshift("ok 3");
say $array[2];
say $array[3];
say $array[0];
print "ok ";
say $array.elems;
my $array2 = ::Array.new;
$array2[0] = "ok 5";
say $array2.shift;
if $array2.shift {
    say "not ok 6";
} else {
    say "ok 6";
}
