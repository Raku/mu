use v6;

=pod

This is a test file.  Whee!

=cut

say "1..7";
say "ok 1 # Welcome to Pugs!";

sub cool { fine($_) ~ " # We've got " ~ toys }
sub fine { "ok " ~ $_ }
sub toys { "fun and games!" }

say cool 2;  # and that is it, folks!

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

eval 'if 1 { say "ok 7" }' or say "not ok 7 # TODO if without parens"

