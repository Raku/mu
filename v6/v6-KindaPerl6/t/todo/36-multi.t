class Main {
    say '1..4';

    my sub ab_1_ ($a) {
        say 'not ok 2';
    };

    my sub ab_3_ ($a,$b,$c) {
        say 'ok 3';
    };

    # my &my_multi := Multi.new;
    proto my_multi {};

    &my_multi.long_names = [

        sub ($a,$b) {
            say 'ok 2';
        },

        &ab_1_,
        &ab_3_,
    ];

    # (&my_multi.long_names).push(
    #    sub ($a,$b,$c,$d) {
    #        say 'ok 4';
    #    }
    # );
    multi my_multi ($a,$b,$c,$d) {
        say 'ok 4';
    }

    say '# long_names: ', &my_multi.long_names;

    say 'ok 1 - survived so far';

    say '# Signature: ', &ab_3_.signature;

    my $capture = \( 1, 2 );
    say '# flattened Capture:   ', $capture;
    my_multi( |$capture );

    say '# Param list:';
    my_multi( 42, 43, 44 );

    say '# flattened Array:';
    my @x = [ 1,2,3,4 ];
    my_multi( |@x );

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

