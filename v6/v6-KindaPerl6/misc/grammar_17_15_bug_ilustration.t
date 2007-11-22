grammar MyGrammar {
    token tok1 {
        'a' { say "closure";return 2; }
    };
}
module Main {
    say '1..2';
    MyGrammar.tok1('a',0);
    say 'ok 1';
    say (MyGrammar.tok1('a',0)).bool;
    if MyGrammar.tok1('a',0) {
        say 'ok 2';
    } else {
        say 'not ok 2';
    }
}
#reason:
#ShortCiruit turns return 1 && 2 into &infix:<&&>(sub {return 1},sub {2})
#fix: add -> {return ...} to kp6 and make ShortCircuit use ->


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
