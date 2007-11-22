grammar KindaPerl6::Grammar {
token space :P5 {[[:space:]]}
token word :P5 {[[:word:]]}
token digit :P5 {[[:digit:]]}
token backslash :P5 {\\}
token newline :P5 {(?m)(\n\r?|\r\n?)}
token not_newline :P5 {.}
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
