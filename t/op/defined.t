use v6;

say "1..13";

if (defined(undef)) { say "not ok 1" } else { say "ok 1" }

if (eval 'defined(1)')   { say "ok 2" } else { say "not ok 2" }
if (eval 'defined("")')  { say "ok 3" } else { say "not ok 3" }
if (eval 'defined("a")') { say "ok 4" } else { say "not ok 4" }
if (eval 'defined(0)')   { say "ok 5" } else { say "not ok 5" }

my $foo;
if (eval 'defined($foo)') { say "not ok 6" } else { say "ok 6" }

$foo = 1;
if (eval 'defined(1)')   { say "ok 7" } else { say "not ok 7" }

$foo = "";
if (eval 'defined("")')  { say "ok 8" } else { say "not ok 8" }

$foo = undef;
if (eval 'defined($foo)') { say "not ok 9" } else { say "ok 9" }

$foo = "a";
if (eval 'defined("a")') { say "ok 10" } else { say "not ok 10" }

$foo = 0;
if (eval 'defined(0)')   { say "ok 11" } else { say "not ok 11" }

if (eval 'undef $foo; 1') { 
    say "ok 12 # TODO undef";
    if (eval 'defined($foo)') {
        say "not ok 13" 
    } else { 
        say "ok 13"
    }
} else {
    say "not ok 12 # TODO undef" ;
    say "ok 13 # skip Depends on TODO 12";
}
