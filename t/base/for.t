use v6;

say "1..18";

## for with plain old range operator w/out parens

my $a;
eval 'for 0 .. 5 { $a = $a ~ $_; }';
if ($a eq '012345') { say "1 ok"; } else { say "1 not ok"; }

# ... with 'pointer'

my $b;
eval 'for 0 .. 5 -> { $b = $b ~ $_; }';
if ($b eq '012345') { say "2 ok"; } else { say "2 not ok # TODO for 0 .. 5 -> {}"; }

# ... with , sub

my $c;
eval 'for 0 .. 5, sub { $c = $c ~ $_; }';
if ($c eq '012345') { say "3 ok"; } else { say "3 not ok # TODO for 0 .. 5, sub {}"; }

# ... with referential sub

my $d;
sub some_sub ($arg) { $d = $d ~ $arg; }
eval 'for 0 .. 5, &some_sub;';
if ($d eq '012345') { say "4 ok"; } else { say "4 not ok # TODO for 0 .. 5, &some_sub"; }

## and now with parens around the range operator

my $e;
eval 'for (0 .. 5) { $e = $e ~ $_; }';
if ($e eq '012345') { say "5 ok"; } else { say "5 not ok"; }

# ... with 'pointer'

my $f;
eval 'for (0 .. 5) -> { $f = $f ~ $_; }';
if ($f eq '012345') { say "6 ok"; } else { say "6 not ok"; }

# ... with sub

my $g;
eval 'for (0 .. 5), sub { $g = $g ~ $_; }';
if ($g eq '012345') { say "7 ok"; } else { say "7 not ok # TODO for (0 .. 5), sub {}"; }

# ... with referential sub

my $h;
sub some_sub_2 ($arg) { $h = $h ~ $arg; }
eval 'for (0 .. 5), &some_sub_2;';
if ($h eq '012345') { say "8 ok"; } else { say "8 not ok # TODO for (0 .. 5), &some_sub"; }

## and now for with 'topical' variables

# ... w/out parens

my $i;
eval 'for 0 .. 5 -> $topic { $i = $i ~ $topic; }';
if ($i eq '012345') { say "9 ok"; } else { say "9 not ok # TODO for 0 .. 5 -> $topic {}"; }

# ... with parens

my $j;
eval 'for (0 .. 5) -> $topic { $j = $j ~ $topic; }';
if ($j eq '012345') { say "10 ok"; } else { say "10 not ok"; }


## for with @list operator w/out parens

my @list_k = (0 .. 5);
my $k;
eval 'for @list_k { $k = $k ~ $_; }';
if ($k eq '012345') { say "11 ok"; } else { say "11 not ok"; }

# ... with 'pointer'

my @list_l = (0 .. 5);
my $l;
eval 'for @list_l -> { $l = $l ~ $_; }';
if ($l eq '012345') { say "12 ok"; } else { say "12 not ok # TODO for @list -> {}"; }

# ... with , sub

my @list_m = (0 .. 5);
my $m;
eval 'for @list_m, sub { $m = $m ~ $_; }';
if ($m eq '012345') { say "13 ok"; } else { say "13 not ok # TODO for @list, sub {}"; }

# ... with referential sub

my @list_n = (0 .. 5);
my $n;
sub some_sub ($arg) { $n = $n ~ $arg; }
eval 'for @list_n, &some_sub;';
if ($n eq '012345') { say "14 ok"; } else { say "14 not ok # TODO for @list, &some_sub"; }

## and now with parens around the @list

my @list_o = (0 .. 5);
my $o;
eval 'for (@list_o) { $o = $o ~ $_; }';
if ($o eq '012345') { say "15 ok"; } else { say "15 not ok"; }

# ... with 'pointer'

my @list_p = (0 .. 5);
my $p;
eval 'for (@list_p) -> { $p = $p ~ $_; }';
if ($p eq '012345') { say "16 ok"; } else { say "16 not ok"; }

# ... with sub

my @list_q = (0 .. 5);
my $q;
eval 'for (@list_q), sub { $q = $q ~ $_; }';
if ($q eq '012345') { say "17 ok"; } else { say "17 not ok # TODO for (@list), sub {}"; }

# ... with referential sub

my @list_r = (0 .. 5);
my $r;
sub some_sub_2 ($arg) { $r = $r ~ $arg; }
eval 'for (@list_r), &some_sub_2;';
if ($r eq '012345') { say "18 ok"; } else { say "18 not ok # TODO for (@list), &some_sub"; }

