use v6;

=pod

Number stringification

=cut

say "1..15";


my $a = 1; "$a";
if ($a eq "1") { say "ok 1"; } else { say "not ok 1"; }

my $a = -1; "$a";
if ($a eq "-1") { say "ok 2"; } else { say "not ok 2"; }

my $a = 1.; "$a";
if ($a eq "1") { say "ok 3"; } else { say "not ok 3"; }

my $a = -1.; "$a";
if ($a eq "-1") { say "ok 4"; } else { say "not ok 4"; }

my $a = 0.1; "$a";
if ($a eq "0.1") { say "ok 5"; } else { say "not ok 5"; }

my $a = -0.1; "$a";
if ($a eq "-0.1") { say "ok 6"; } else { say "not ok 6"; }

my $a = 10.01; "$a";
if ($a eq "10.01") { say "ok 7"; } else { say "not ok 7"; }

my $a = 1e3; "$a";
if ($a eq "1000") { say "ok 8"; } else { say "not ok 8"; }

my $a = 10.01e3; "$a";
if ($a eq "10010" ) { say "ok 9"; } else { say "not ok 9"; }

my $a = 0b100; "$a";
if ($a eq "4") { say "ok 10"; } else { say "not ok 10"; }

my $a = 0x100; "$a";
if ($a eq "256") { say "ok 11"; } else { say "not ok 11"; }

my $a = 1; "$a"; 
if ($a + 1 == 2  ) { say "ok 12"; } else { say "not ok 12"; }

my $a = -1; "$a";
if ($a + 1 == 0) { say "ok 13"; } else { say "not ok 13"; }

my $a = 80000.0000000000000000000000000;
if ($a == 80000.0 ) { say "ok 14"; } else { say "not ok 14"; }

my $a = 1.0000000000000000000000000000000000000000000000000000000000000000000e1;
if ($a == 10.0) { say "ok 15"; } else { say "not ok 15"; }
