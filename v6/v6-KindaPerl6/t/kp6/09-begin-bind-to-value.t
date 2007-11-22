use v6-alpha;

module Main {

say '1..2';

my $counter;

my $var =
BEGIN {
    $counter := 1;
    my $lex = 2;
};

if ($counter) {
    say "ok ", $counter;
} else {
    say "not ok 1"
};
if ($var) {
    say "ok ", $var;
} else {
    say "not ok 2"
};

}

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

