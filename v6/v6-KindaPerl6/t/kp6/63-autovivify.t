use v6-alpha;

say "1..7";

my $v1;
$v1 = 1;
say "ok 1 - assignment";

my $v2;
$v2 := 1;
say "ok 2 - bind";

my $v31;
$v3 + 1;
say "ok 3 - fetch from autovivified hash";

my $v3;
$v3{1} = 1;
say "ok 4 - assignment to autovivified hash";

my $v4;
$v4{1} := 1;
say "ok 5 - bind to autovivified hash";

my $v5;
$v5[1] = 1;
say "ok 6 - assignment to autovivified array";

my $v6;
$v6[1] := 1;
say "ok 7 - bind to autovivified array";


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

