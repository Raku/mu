use v6;

say "1..6";

# test all variants of join() 

my $a = ["a", "b", "c"].join("|");
if ($a eq "a|b|c") { say "ok 1"; } else { say "not ok 1 # TODO [].join()"; }

my $b = ("a", "b", "c").join("|");
if ($b eq "a|b|c") { say "ok 2"; } else { say "not ok 2 # TODO ().join()"; }

my @list_c = ("a", "b", "c");
my $c = @list_c.join("|");
if ($c eq "a|b|c") { say "ok 3"; } else { say "not ok 3 # TODO @list.join()"; }

my @list_d = ("a", "b", "c");
my $d = join("|", @list_d);
if ($d eq "a|b|c") { say "ok 4"; } else { say "not ok 4"; }

my $e = join("|", "a", "b", "c");
if ($e eq "a|b|c") { say "ok 5"; } else { say "not ok 5"; }

my $f = join("|", [ "a", "b", "c" ]);
if ($f eq "a|b|c") { say "ok 6"; } else { say "not ok 6"; }

