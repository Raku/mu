use v6-alpha;

say "1..4";

my @a;
my @b;

@a = (1,2,3);
if @a.perl ne "[ 1, 2, 3 ]" { print "not " }
say "ok 1 - array = list";

@a = [1,2,3];
if @a.perl ne "[ [ 1, 2, 3 ] ]" { print "not " }
say "ok 2 - array = [list]";

@b = (1,2,3);
@a = @b;
if @a.perl ne "[ 1, 2, 3 ]" { print "not " }
say "ok 3 - array = array";

@b = (1,2,3);
@a = [@b];
if @a.perl ne "[ [ 1, 2, 3 ] ]" { print "not " }
say "ok 4 - array = [array]";


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

