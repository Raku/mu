package Grammar;

=head1 NAME

Grammar

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

This packages provides basic matching functions for the AST.

=head1 FUNCTIONS

=cut

=head2 space

Matches white space as defined in perl5 as [[:space:]]

=cut

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

=head2 digit

Matches digits as defined in perl5 as [[:digit:]]

=cut

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

=head2 word

Matches a word as defined in perl5 as [[:word:]]

See for more info:
L<http://perldoc.perl.org/perlretut.html#More-on-characters%2c-strings%2c-and-character-classes>

=cut

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

=head2 backslash

Matches a \

=cut

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

=head2 newline

Matches a newline as being \n\r, or \r\n, or \r, or \n (I think).

TODO: Should 10 and 13 be \n and \r to be uni-code compatiable?

=cut

sub newline {
    my $grammar = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

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

=head2 not_newline

Returns $MATCH if  the next character is a newline type of character
10 or 13

TODO: Should 10 and 13 be \n and \r to be uni-code compatiable?

=cut

sub not_newline {
    my $grammar = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new(
        'str' => $str,'from' => $pos,'to' => $pos, 'bool' => 0
    );

    return $MATCH if ord( substr($str, $MATCH->to()) ) == 10
        || ord( substr($str, $MATCH->to()) ) == 13;

    $MATCH->to( 1 + $MATCH->to );

    $MATCH->bool( 1 );

    $MATCH;
}

1;

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

=cut
