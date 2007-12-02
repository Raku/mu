say '1..4';

proto my_multi {};

multi my_multi($a) {
    say 'not ok 2';
};
multi my_multi($a,$b,$c) {
    say 'ok 3';
};
multi my_multi ($a,$b) {
    say 'ok 2';
}
multi my_multi ($a,$b,$c,$d) {
    say 'ok 4';
}

say 'ok 1 - survived so far';

my $capture = \( 1, 2 );
my_multi( |$capture );

my_multi( 42, 43, 44 );

my @x = ( 1,2,3,4 );
my_multi( |@x );


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

