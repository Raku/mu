use v6;

say "1..10";

my %hash1 = ('key' => 'value');
if (%hash1{'key'} eq 'value') { say "ok 1" } else { say "not ok 1" }

my %hash2;
eval '%hash2 = (:one, :key<value>, :three(3))';
if (%hash2{'one'} == 1) { say "ok 2" } else { say "not ok 2 # TODO colonpair" }
if (%hash2{'key'} eq 'value') { say "ok 3" } else {
    say "not ok 3 # TODO colonpair" 
}
if (%hash2{'three'} == 3) { say "ok 4" } else {
    say "not ok 4 # TODO colonpair" 
}

my $value;
eval '$value = %hash1<key>';
if ($value eq 'value') { say "ok 5" } else { say "not ok 5 # TODO %hash<>" }

my @slice1 = %hash2{"one", "three"};
if (@slice1[0] == 1) { say "ok 6" } else { say "not ok 6 # TODO hash slice" }
if (@slice1[1] == 3) { say "ok 7" } else { say "not ok 7 # TODO hash slice" }

my @slice2;
@slice2 = eval '%hash2<three one>';
if (@slice2[0] == 5123123) { say "not ok 8 wth?" } else { say "ok 8" }
if (@slice2[0] == 3) { say "ok 9" } else { say "not ok 9 # TODO %hash<>" }
if (@slice2[1] == 1) { say "ok 10" } else { say "not ok 10 # TODO %hash<>" }

