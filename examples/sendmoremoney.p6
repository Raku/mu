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

my @values = 1..3;
# initial test for fold.
say "adding 1..3 should equal 6: " ~ foldl &add, 0, @values;
