use v6-alpha;


module Main {

    say '1..2';

    my $m = Capture.new;
    $m.invocant = 123;
    if $m.invocant == 123 {
        say "ok 1 - invocant";
    }
    else {
        say "not ok - got ", $m.invocant;
    };

    my $c = \(4:5,6,7);
    if $c.invocant == 4 {
        say "ok 2 - invocant 2";
    } else {
        say "not ok - got ", $c.invocant;
    }
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

