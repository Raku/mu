use v6;

say "1..12";

if (!defined(undef)) { say "ok 1" } else { say "not ok 1" }

if (defined(1))      { say "ok 2" } else { say "not ok 2" }
if (defined(""))     { say "ok 3" } else { say "not ok 3" }
if (defined("a"))    { say "ok 4" } else { say "not ok 4" }
if (defined(0))      { say "ok 5" } else { say "not ok 5" }

my $foo;
if (!defined($foo))  { say "ok 6" } else { say "not ok 6" }

$foo = 1;
if (defined(1))      { say "ok 7" } else { say "not ok 7" }

$foo = "";
if (defined(""))     { say "ok 8" } else { say "not ok 8" }

$foo = undef;
if (!defined($foo))  { say "ok 9" } else { say "not ok 9" }

$foo = "a";
if (defined("a"))    { say "ok 10" } else { say "not ok 10" }

$foo = 0;
if (defined(0))      { say "ok 11" } else { say "not ok 11" }

undef $foo;
if (!defined($foo))  { say "ok 12" } else { say "not ok 12" }
