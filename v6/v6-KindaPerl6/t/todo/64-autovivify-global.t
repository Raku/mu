use v6-alpha;

say "1..6";

$X::v1 = 1;
say "ok 1 - assignment";

$X::v2 := 1;
say "ok 2 - bind";

$X::v3{1} = 1;
say "ok 3 - assignment to autovivified hash";

$X::v4{1} := 1;
say "ok 4 - bind to autovivified hash";

$X::v5[1] = 1;
say "ok 5 - assignment to autovivified array";

$X::v6[1] := 1;
say "ok 6 - bind to autovivified array";


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

