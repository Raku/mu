module Main {
    say '1..3';

    my sub sort_sub ($a, $b) {
           $a <=> $b
    };

    my @l;
    @l[0] = 2;
    @l[1] = 1;
    @l[2] = 3;

    my @sorted;
    @sorted = @l.sort(&sort_sub);
    say 'ok ' ~ @sorted[0];
    say 'ok ' ~ @sorted[1];
    say 'ok ' ~ @sorted[2];

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

