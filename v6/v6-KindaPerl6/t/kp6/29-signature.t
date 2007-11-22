class Main {
    my sub ab ($a,$b) {
        if ($a == $b) {
            say "ok";
        } else {
            say "not ok # "~$a~" != "~$b;;
        }
    };

    say '1..2';
    say 'ok 1';

    say '# Signature: ', &ab.signature;
    if (&ab.signature).arity == 2 {
        say "ok 2 arity";
    }
    else {
        say "not ok 2"
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

