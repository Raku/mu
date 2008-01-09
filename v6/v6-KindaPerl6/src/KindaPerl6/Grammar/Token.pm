use v6-alpha;

grammar KindaPerl6::Grammar {

token token_p5_modifier {
    ':P5'|':Perl5'
}
token token_p5_body {
    |  \\ .  <token_p5_body>
    |  <!before \} > . <token_p5_body>
    |  ''
};
token token_P5 {
    token <.ws> <opt_name> <.opt_ws> <token_p5_modifier> <.opt_ws> \{
        <token_p5_body>
    \}
    {
        make Token.new(
             'name' => $$<opt_name>,
             'regex' => P5Token.new(regex => $$<token_p5_body>),
             'sym' => undef
        );
    }
};

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
