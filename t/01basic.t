use v6;

=pod

Basic tests.

=cut

say "1..14";
say "ok 1 # Welcome to Pugs!";

sub cool { fine($_) ~ " # We've got " ~ toys }
sub fine { "ok " ~ $_ }
sub toys { "fun and games!" }

(2).cool.say;  # and that is it, folks!

my $foo = "Foo";
eval 'undef $foo';
if (!$foo) { say 'ok 3' } else { say 'not ok 3 # TODO' }

my $bar;
eval ' unless ($foo) { $bar = "true"; } ';
if ($bar) { say 'ok 4' } else { say 'not ok 4 # TODO' }

my ($var1, $var2) = ("foo", "bar");
if ($var1 eq $var2) { say 'not ok 5 # TODO' } else { say 'ok 5' }
if (eval '(my $quux = 1) == 1)') { say "ok 6" } else { 
    say "not ok 6 # TODO my returns LHS"
}

eval 'if 1 { say "ok 7" }' or say "not ok 7 # TODO if without parens";
eval 'for 1 { say "ok 8" }' or say "not ok 8 # TODO for without parens";
eval 'while (0) { } say "ok 9"' or say "not ok 9 # TODO while";

my $lasttest = 0;
eval 'for (1..10) { $lasttest++; last; $lasttest++; }';
if ($lasttest == 1) { say "ok 10" } else { say "not ok 10 # TODO last" }

my $nexttest = 0;
eval 'for (1..10) { $nexttest++; next; $nexttest++; }';
if ($nexttest == 10) { say "ok 11" } else { say "not ok 11 # TODO next" }

print "ok ";
if (eval '12.print') { say "\nok 13" } else { say "\nnot ok 13 # TODO 5.say" }

if (!eval 'say(1 ?? "ok 14" :: "Bail out!")') { say "not ok 14" }
