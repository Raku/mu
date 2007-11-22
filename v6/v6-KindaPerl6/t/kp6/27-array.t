use v6-alpha;
module Main {
    say "1..6";
    my @array;
    @array[0] = 1;
    @array[1] = 2;
    say "ok "~@array[0];
    say "ok "~@array[1];
    say "ok " ~ ( 1 + @array.elems );
    my @array2;
    @array2[0] = "4";
    @array2[1] = "5";
    @array2.map(sub ($elem) {say "ok "~$elem});
    if @array.join( ',' ) eq '1,2' {
        say "ok 6";
    } else {
        say "not ok";
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

