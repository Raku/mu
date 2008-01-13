say "1..4";
my $foo;
if ($foo) {
    say "not ok 1";
} else {
    say "ok 1";
};
$foo = undef;
if ($foo) {
    say "not ok 2";
} else {
    say "ok 2";
};
my %bar;
if (%bar{'no existent key'}) {
    say "not ok 3";
} else {
    say "ok 3";
};
my @baz;
if (@baz[666]) {
    say "not ok 4";
} else {
    say "ok 4";
};

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

