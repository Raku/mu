grammar MyGrammar {
    token word :P5 {[[:word:]]};
};
module Main {
    say '1..3';
    $_ = 'a';
    if MyGrammar.word() {
        say 'ok 1';
    } else {
        say 'not ok 1';
    }
    $_ = '';
    if MyGrammar.word() {
        say 'not ok 2';
    } else {
        say 'ok 2';
    }
    $_ = '1';
    if MyGrammar.word() {
        say 'ok 3';
    } else {
        say 'not ok 3';
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

