sub qsort($array) {
    if $array.elems == 0 {
        ::Array.new;
    } else {
        my $partition = $array.[0];
        my $left  = qsort(grep sub ($elem) {$elem < $partition},$array);
        my $right = qsort(grep sub ($elem) {$elem > $partition},$array);

        my $result = ::Array.new;
        map(sub ($x) {$result.push($x.FETCH)},$left);
        $result.push($partition.FETCH);
        map(sub ($x) {$result.push($x.FETCH)},$right);
        $result;
    }
}
say "started";
my $array = ::Array.new;
$array.push(3);
$array.push(1);
$array.push(5);
$array.push(2);
$array.push(4);
$array.push(7);
my $array2 = qsort($array);
map(sub ($x) {say $x},$array2);
