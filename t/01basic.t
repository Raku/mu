use v6;

=pod

This is a test file.  Whee!

=cut

say "1..2";
say "ok 1 # Welcome to Pugs!";

sub cool { fine($_) ~ " # We've got " ~ toys }
sub fine { "ok " ~ $_ }
sub toys { "fun and games!" }

say cool 2 # and that's it, folks!
