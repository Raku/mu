use v6;
use Test;

# Test the Utable data structure

plan 1190;

ok eval('use Utable'), 'Can load the module';
ok my $u = Utable.new, 'Utable.new works';
is $u.contains(42), False, '.contains works on empty table';

$u = undef;
ok $u = Utable.new(@@( 1..2 ; 5..8 )), 'Utable.new with a list of ranges works';
for 1..2, 5..8 -> $n {
    is $u.contains($n), True, ".contains finds $n in @@( 1..2 ; 5..8 )";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.contains($n), False, ".contains doesn't find $n in @@( 1..2 ; 5..8 )";
}
my $s1 = $u.tostr;
is $s1, '1..2;5..8', '.tostr works for @@( 1..2 ; 5..8 )';
ok $u eqv $u.perl.eval, 'round-trip to .perl works for @@( 1..2 ; 5..8 )';

$u = undef;
ok $u = Utable.new('1..2;5..8'), 'Utable.new with a string works';
for 1..2, 5..8 -> $n {
    is $u.contains($n), True, ".contains finds $n in '1..2;5..8'";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.contains($n), False, ".contains doesn't find $n in '1..2;5..8'";
}
my $s2 = $u.tostr;
is $s2, '1..2;5..8', ".tostr works for '1..2;5..8'";
ok $u eqv $u.perl.eval, "round-trip to .perl works for '1..2;5..8'";

is $u.inverse.tostr, '0;3..4;9..1114111', '.inverse works';

my $io = open "/tmp/utable.$*PID", :w orelse die;
$u.print($io);
$io.close orelse die;
my $s = slurp "/tmp/utable.$*PID";
is $s, '1..2;5..8', '.print($io) works';
unlink "/tmp/utable.$*PID" orelse die;

my $io = open "/tmp/utable.$*PID", :w orelse die;
$u.print(:$io);
$io.close orelse die;
my $s = slurp "/tmp/utable.$*PID";
is $s, '1..2;5..8', '.print(:$io) works';
unlink "/tmp/utable.$*PID" orelse die;

my $io = open "/tmp/utable.$*PID", :w orelse die;
$u.say($io);
$io.close orelse die;
my $s = slurp "/tmp/utable.$*PID";
is $s, "1..2;5..8\n", '.say($io) works';
unlink "/tmp/utable.$*PID" orelse die;

my $io = open "/tmp/utable.$*PID", :w orelse die;
$u.say(:$io);
$io.close orelse die;
my $s = slurp "/tmp/utable.$*PID";
is $s, "1..2;5..8\n", '.say(:$io) works';
unlink "/tmp/utable.$*PID" orelse die;

$u = undef;
ok $u = Utable.new(@@( 1..2 ; 5..8 ), :val(1, 2)), 'Utable.new with a list of ranges and a list of values works';
for 1..2 -> $n {
    is $u.get($n), 1, ".get finds 1 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for 5..8 -> $n {
    is $u.get($n), 2, ".get finds 2 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.get($n), undef, ".get returns undef for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
my $s1 = $u.tostr;
is $s1, '1..2:1;5..8:2', '.tostr works for @@( 1..2 ; 5..8 ), (1, 2)';
ok $u eqv $u.perl.eval, 'round-trip to .perl works for @@( 1..2 ; 5..8 ), (1, 2)';

$u = undef;
ok $u = Utable.new('1..2:1;5..8:2'), 'Utable.new with a string with values works';
for 1..2 -> $n {
    is $u.get($n), 1, ".get finds 1 for $n in '1..2:1;5..8:2'";
}
for 5..8 -> $n {
    is $u.get($n), 2, ".get finds 2 for $n in '1..2:1;5..8:2'";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.get($n), undef, ".get returns undef for $n in '1..2:1;5..8:2'";
}
my $s2 = $u.tostr;
is $s2, '1..2:1;5..8:2', ".tostr works for '1..2:1;5..8:2'";
ok $u eqv $u.perl.eval, "round-trip to .perl works for '1..2:1;5..8:2'";

$u = Utable.new;
for 1..2, 5..8 -> $n { $u.add($n); }
# test redundant adds
for 1..2, 5..8 -> $n { $u.add($n); }
for 1..2, 5..8 -> $n {
    is $u.contains($n), True, "(add) .contains finds $n in @@( 1..2 ; 5..8 )";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.contains($n), False, "(add) .contains doesn't find $n in @@( 1..2 ; 5..8 )";
}
my $s1 = $u.tostr;
is $s1, '1..2;5..8', '(add) .tostr works for @@( 1..2 ; 5..8 )';
ok $u eqv $u.perl.eval, '(add) round-trip to .perl works for @@( 1..2 ; 5..8 )';

$u = Utable.new;
for 8, 7, 6, 5, 2, 1 -> $n { $u.add($n); }
# test redundant adds
for 1..2, 5..8 -> $n { $u.add($n); }
for 1..2, 5..8 -> $n {
    is $u.contains($n), True, "(backwards add) .contains finds $n in @@( 1..2 ; 5..8 )";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.contains($n), False, "(backwards add) .contains doesn't find $n in @@( 1..2 ; 5..8 )";
}
my $s1 = $u.tostr;
is $s1, '1..2;5..8', '(backwards add) .tostr works for @@( 1..2 ; 5..8 )';
ok $u eqv $u.perl.eval, '(backwards add) round-trip to .perl works for @@( 1..2 ; 5..8 )';

$u = Utable.new;
for 2, 6, 7, 5, 1, 8 -> $n { $u.add($n); }
# test redundant adds
for 1..2, 5..8 -> $n { $u.add($n); }
for 1..2, 5..8 -> $n {
    is $u.contains($n), True, "(random add) .contains finds $n in @@( 1..2 ; 5..8 )";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.contains($n), False, "(random add) .contains doesn't find $n in @@( 1..2 ; 5..8 )";
}
my $s1 = $u.tostr;
is $s1, '1..2;5..8', '(random add) .tostr works for @@( 1..2 ; 5..8 )';
ok $u eqv $u.perl.eval, '(random add) round-trip to .perl works for @@( 1..2 ; 5..8 )';

$u = Utable.new;
for 1..2 -> $n { $u.add($n, :val(1)); }
for 5..8 -> $n { $u.add($n, :val(2)); }
# test redundant adds
for 1..2 -> $n { $u.add($n, :val(1)); }
for 5..8 -> $n { $u.add($n, :val(2)); }
for 1..2 -> $n {
    is $u.get($n), 1, "(add) .get finds 1 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for 5..8 -> $n {
    is $u.get($n), 2, "(add) .get finds 2 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.get($n), undef, "(add) .get returns undef for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
my $s1 = $u.tostr;
is $s1, '1..2:1;5..8:2', '(add) .tostr works for @@( 1..2 ; 5..8 ), (1, 2)';
ok $u eqv $u.perl.eval, '(add) round-trip to .perl works for @@( 1..2 ; 5..8 ), (1, 2)';

$u = Utable.new;
for 8, 7, 6, 5 -> $n { $u.add($n, :val(2)); }
for 2, 1 -> $n { $u.add($n, :val(1)); }
# test redundant adds
for 1..2 -> $n { $u.add($n, :val(1)); }
for 5..8 -> $n { $u.add($n, :val(2)); }
for 1..2 -> $n {
    is $u.get($n), 1, "(backwards add) .get finds 1 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for 5..8 -> $n {
    is $u.get($n), 2, "(backwards add) .get finds 2 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.get($n), undef, "(backwards add) .get returns undef for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
my $s1 = $u.tostr;
is $s1, '1..2:1;5..8:2', '(backwards add) .tostr works for @@( 1..2 ; 5..8 ), (1, 2)';
ok $u eqv $u.perl.eval, '(backwards add) round-trip to .perl works for @@( 1..2 ; 5..8 ), (1, 2)';

$u = Utable.new;
for 2, 1 -> $n { $u.add($n, :val(1)); }
for 5, 8, 6, 7 -> $n { $u.add($n, :val(2)); }
# test redundant adds
for 1..2 -> $n { $u.add($n, :val(1)); }
for 5..8 -> $n { $u.add($n, :val(2)); }
for 1..2 -> $n {
    is $u.get($n), 1, "(random add) .get finds 1 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for 5..8 -> $n {
    is $u.get($n), 2, "(random add) .get finds 2 for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
for -2..0, 3..4, 9..11 -> $n {
    is $u.get($n), undef, "(random add) .get returns undef for $n in @@( 1..2 ; 5..8 ), (1, 2)";
}
my $s1 = $u.tostr;
is $s1, '1..2:1;5..8:2', '(random add) .tostr works for @@( 1..2 ; 5..8 ), (1, 2)';
ok $u eqv $u.perl.eval, '(random add) round-trip to .perl works for @@( 1..2 ; 5..8 ), (1, 2)';

is eval('do{$u.add(0, :val(2)); $u.tostr}'), undef, 'add overlapping with different value fails';
is eval('do{$u.add(3, :val(4)); $u.tostr}'), undef, 'add overlapping with different value fails';
is eval('do{$u.add(4, :val(8)); $u.tostr}'), undef, 'add overlapping with different value fails';
is eval('do{$u.add(9, :val(0)); $u.tostr}'), undef, 'add overlapping with different value fails';
is eval('do{$u.add(3, :val(-1)); $u.tostr}'), undef, 'add overlapping with different value fails';

$u = Utable.new;
my %h;
for ^1000 {
    my $n = (0..$unicode_max).pick while %h{$n};
    my $val = (1..2).pick;
    $u.add($n, :$val);
    %h{$n} = $val;
}
for %h.keys -> $n {
    is $u.get($n), %h{$n}, 'rand lookup succeeds';
}
ok $u eqv $u.perl.eval, 'round-trip to .perl works for big rand table';

$u = undef;
ok $u = Utable.new(@@( 1..2 ; 3..4 ; 5..8 )), 'Utable.new with a list of contiguous ranges works';
is $u.tostr, '1..8', '$.preen connects contiguous ranges';
$u = undef;
ok $u = Utable.new(@@( 1..-2 ; 3..-4 ; 5..-8 )), 'Utable.new with a list of contiguous ranges works';
is $u.tostr, '', '$.preen deletes null ranges';
$u = undef;
ok $u = Utable.new(@@( 1..-2 ; 3..4 ; 5..-8 )), 'Utable.new with a list of contiguous ranges works';
is $u.tostr, '3..4', '$.preen deletes null ranges, leaving non-null ones';

$u = undef;
ok $u = Utable.new(@@( 1..2 ; 3..4 ; 5..8 ), :val(1, 2, 3)), 'Utable.new with a list of contiguous ranges with different values works';
is $u.tostr, '1..2:1;3..4:2;5..8:3', '$.preen leaves contiguous ranges with different values';
$u = undef;
ok $u = Utable.new(@@( 1..2 ; 3..4 ; 5..8 ), :val(1, 1, 1)), 'Utable.new with a list of contiguous ranges with same values works';
is $u.tostr, '1..8:1', '$.preen connects contiguous ranges with same values';
$u = undef;
ok $u = Utable.new(@@( 1..2 ; 3..4 ; 5..8 ), :val(1, 1, 2)), 'Utable.new with a list of contiguous ranges with values works';
is $u.tostr, '1..4:1;5..8:2', '$.preen connects contiguous ranges with same values, leaving different values unconnected';
$u = undef;
ok $u = Utable.new(@@( 1..-2 ; 3..-4 ; 5..-8 ), :val(1, 2, 3)), 'Utable.new with a list of contiguous ranges with values works';
is $u.tostr, '', '$.preen deletes null ranges with values';
$u = undef;
ok $u = Utable.new(@@( 1..-2 ; 3..4 ; 5..-8 ), :val(1, 2, 3)), 'Utable.new with a list of contiguous ranges with values works';
is $u.tostr, '3..4:2', '$.preen deletes null ranges, leaving non-null ones with values';
