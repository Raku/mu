use v6;

my $s; 
my $e; 
my $n; 
my $d; 
my $m; 
my $o; 
my $r;
my $y;

sub foldl(Code &op, Any $initial, *@values) returns Any {
    if (+@values == 0) {
         return $initial;
    } else {
         return &op(shift @values, &?SUB(&op, $initial, @values));
    }
}

sub add(Int $x, Int $y) returns Int {
    return $x + $y;
}

sub construct(*@values) returns Int {
    return foldl -> $x, $y { $x * 10 + $y}, 0, @values;
}

$s = any(0..10) & none(0);
$e = any(0..10);
$n = any(0..10);
$d = any(0..10);
$m = any(0..10) & none(0);
$o = any(0..10);
$r = any(0..10);
$n = any(0..10);
$y = any(0..10);

my $send := construct($s,$e,$n,$d);
my $more := construct($m,$o,$r,$e);
my $money := construct($m,$o,$n,$e,$y);

if ($send + $more == $money) {
	say " send = $send";
	say "+more = $more";
	say "-------------"
	say "money = $money";
}
