use v6;
say "1..10";

if ("a" eq "a")  { say "ok 1" } else { say "not ok 1" }
if ("a" eq "ab") { say "not ok 2" } else { say "ok 2" }

if ("a" ne "a")  { say "not ok 3" } else { say "ok 3" }
if ("a" ne "ab") { say "ok 4" } else { say "not ok 4" }

if ("\0" eq "\0") { say "ok 5" } else { say "not ok 5" }

my $foo;
if ($foo eq "") { say "ok 6" } else { say "not ok 6" }
if ($foo eq "f") { say "not ok 7" } else { say "ok 7" }

my @foo;
if (@foo[0] eq "") { say "ok 8" } else { say "not ok 8" }
if (@foo[0] eq "f") { say "not ok 9" } else { say "ok 9" }

@foo = eval '';
if (@foo[0] eq "f") { say "not ok 10" } else { say "ok 10" }
