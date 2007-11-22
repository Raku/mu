

class Main {
    say '1..8';

    my $junc = all( 1,2,3,4);

    say '# junction: ', $junc.Str;
    say '# junction: ', $junc;

    # $junc.say;

    my sub sayok ( $a ) {
        ( 'ok ' ~ $a ).say;
    };

    sayok( $junc );

    if $junc {
        say "ok 5";
    }
    else {
        say "not ok 5";
    };

    if all( 1, 0 ) {
        say "not ok 6";
    }
    else {
        say "ok 6";
    };

    if (1|0) {
        say "ok 7";
    }
    else {
        say "not ok 7";
    };

    if (1&0) {
        say "not ok 8";
    }
    else {
        say "ok 8";
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

