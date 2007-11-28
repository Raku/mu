class Main {
    say '1..4';

    my @source;
    @source[0] = 2;
    @source[1] = 3;
    @source[2] = 5;
    @source[3] = 7;

    my @mapped;
    @mapped = @source.map(sub { "prime:" ~ $_ });
    if (@mapped[0] ne "prime:" ~ @source[0]) { print "not "; }; say "ok 1 - ", @mapped[0];
    if (@mapped[1] ne "prime:" ~ @source[1]) { print "not "; }; say "ok 2";
    if (@mapped[2] ne "prime:" ~ @source[2]) { print "not "; }; say "ok 3";
    if (@mapped[3] ne "prime:" ~ @source[3]) { print "not "; }; say "ok 4";

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

