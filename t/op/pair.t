use v6;

say "1..13";

my $pair = 'foo' => 'bar';
if (ref $pair eq 'Pair') { say "ok 1" } else { say "not ok 1" }

my $foo = $pair.key;
my $bar = $pair.value;

if ($foo eq 'foo') { say "ok 2" } else { say "not ok 2" }
if ($bar eq 'bar') { say "ok 3" } else { say "not ok 3" }

my @pair1 = $pair.kv;

if (@pair1[0] eq 'foo') { say "ok 4" } else { say "not ok 4" }
if (@pair1[1] eq 'bar') { say "ok 5" } else { say "not ok 5" }

my $quux = eval '(quux => "xyzzy").key';

if ($quux eq 'quux') { say "ok 6 # TODO => lhs quotes" } else { say "not ok 6 # TODO => lhs quotes" }

#Pair with a numeric value
my $pair = 'foo' => 2;
if (ref $pair eq 'Pair') { say "ok 7" } else { say "not ok 7" }
my $two = $pair.value;
if ($two == 2) { say "ok 9" } else { say "not ok 9" };

#Pair with a Pair value
my $pair = "foo" => ("bar" => "baz");
if (ref $pair eq 'Pair') { say "ok 10" } else { say "not ok 10" }
my $pair2 = $pair.value;
if (ref $pair2 eq 'Pair') { say "ok 11" } else { say "not ok 11" }

#Pair with a Pair key
$pair = ("foo" => "bar") => "baz";
if (ref $pair eq 'Pair') { say "ok 12" } else { say "not ok 12" }
my $key = $pair.key;
if (ref $key eq 'Pair') { say "ok 13" } else { say "not ok 13" }

#Pair list a la http://www.nntp.perl.org/group/perl.perl6.language/19360
my $list = 1 => 2 => 3 => 4;
