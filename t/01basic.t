use v6;

=pod

This is a test file.  Whee!

=cut

say "1..4";
say "ok 1 # Welcome to Pugs!";

sub cool { fine($_) ~ " # We've got " ~ toys }
sub fine { "ok " ~ $_ }
sub toys { "fun and games!" }

say cool 2;  # and that's it, folks!

my $foo = "Foo";
eval 'undef $foo';
if (!$foo) { say 'ok 3' } else { say 'not ok 3 # TODO' }

my $bar;
eval ' unless ($foo) { $bar = "true"; } ';
if ($bar) { say 'ok 4' } else { say 'not ok 4 # TODO' }

