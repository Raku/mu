grammar MyGrammar {
    token digit :P5 {[[:digit:]]};
    token digits { \d+ };
    token val_int {
        <digits>
        { return ::Val::Int( 'int' => ~$/ ) }
    };
};
class Val::Int {
    has $.int;
    method true {
        1;
    }
};

say '1..1';
$_ = '1';
say 'ok ' ~ MyGrammar.val_int();

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

