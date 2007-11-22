package Grammar;

    sub space {
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
        my $MATCH;
        $MATCH = KindaPerl6::Perl5::Match->new(
            'str' => $str,'from' => $pos,'to' => $pos, );
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:space:]])/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }
    sub digit {
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
            'str' => $str,'from' => $pos,'to' => $pos, );
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:digit:]])/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }

    sub word {
            my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
            my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
                'str' => $str,'from' => $pos,'to' => $pos, );
            $MATCH->bool(
                substr($str, $MATCH->to()) =~ m/^([[:word:]])/
                ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
                : 0
            );
            $MATCH;
    }
    sub backslash {
            my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
            my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
                'str' => $str,'from' => $pos,'to' => $pos, );
            $MATCH->bool(
                substr($str, $MATCH->to(), 1) eq '\\'         # '
                ? ( 1 + $MATCH->to( 1 + $MATCH->to() ))
                : 0
            );
            $MATCH;
    }

    sub newline {
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
            'str' => $str,'from' => $pos,'to' => $pos, );
        return $MATCH unless ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/(?m)^(\n\r?|\r\n?)/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }
    sub not_newline {
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2];
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
            'str' => $str,'from' => $pos,'to' => $pos, 'bool' => 0 );
        return $MATCH if ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->to( 1 + $MATCH->to );
        $MATCH->bool( 1 );
        $MATCH;
    }

1;

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
