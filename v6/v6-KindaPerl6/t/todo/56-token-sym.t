class Main {

say "1..1";

method ok { say "ok 1" };

proto token xyz {};

token xyz:sym<abc>   { abc }
token xyz:abf        { <Main.ok> abf }
token xyz:sym<abger> { abger }
token xyz:sym« << »  { << }

$_ = "abfbbb";
# Main.xyz;   XXX - AST bug
xyz( $_, 0 );  # XXX - what is the calling convention for regexes?

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

