grammar MyGrammar {
    token always_true {
        ''
    };
    token myTok {
        'a' <always_true> 'b' <always_true> 'c'
    };
};
module Main {
    say '1..1';
    $_='abc';
    if MyGrammar.myTok() {
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

