#!perl6
use v6;

die "*** This example is currently broken -- needs more work.";

my $s = any(0..9) & none(0);
my $e = any(0..9);
my $n = any(0..9);
my $d = any(0..9);
my $m = any(0..9) & none(0);
my $o = any(0..9);
my $r = any(0..9);
my $n = any(0..9);
my $y = any(0..9);



sub foldl (Code &op, Any $initial, *@values) returns Any {
    if (+@values == 0) {
         return $initial;
    } else {
         return &op(shift @values, &?SUB(&op, $initial, @values));
    }
}

sub add (Int $x, Int $y) returns Int {
    return $x + $y;
}

sub construct (*@values) returns Junction {
    return foldl( -> $x, $y { $x * 10 + $y}, 0, @values);
}


if (any($s,$e,$n,$d,$m,$o,$r,$y) == one($s,$e,$n,$d,$m,$o,$r,$y)) {

    my $send := construct($s,$e,$n,$d);
    my $more := construct($m,$o,$r,$e);
    my $money := construct($m,$o,$n,$e,$y);

    if ($send + $more == $money) {
        say " send = $send";
        say "+more = $more";
        say "-------------";
        say "money = $money";
    }
}
