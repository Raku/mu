module Main {
    say '1..4';

    sub grep_sub ($a) {
        $a == 1;
    };

    my @l;
    @l[0] = 2;
    @l[1] = 1;

    my @grepped;
    @grepped = @l.grep(&grep_sub);
    say 'ok ' ~ @grepped[0];
    say 'ok ' ~ @l[0];
    # 2 elements in @l ...
    if (@l[1]) {
        say "ok 3";
    } else {
        say "not ok 3";
    };
    # ... but not 2 elements in @grepped
    if (@grepped[1]) {
        print "not ";
    };
    say "ok 4";
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

