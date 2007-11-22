grammar MyGrammar {
    token word :P5 {[[:word:]]};
    token ident_digit {
          <.word> <.ident_digit>
        | ''
    };
};
module Main {
    say '1..1';
    $_ = 'a123';
    if MyGrammar.ident_digit() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
};

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

