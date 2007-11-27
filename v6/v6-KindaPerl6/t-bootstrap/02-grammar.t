
use lib "compiled/perl5-kp6-kp6/lib";
use strict;
use KindaPerl6::Runtime::Perl5::Runtime;
use KindaPerl6::Grammar::Quote;
use KindaPerl6::Grammar;
use Test::More tests => 2;

$_ = ::DISPATCH( $::Scalar, "new" );
my $MATCH;

::DISPATCH_VAR( $_, 'STORE', ::DISPATCH( $::Str, 'new', '123' ) );
$MATCH = ::DISPATCH( $::KindaPerl6::Grammar, 'term' );
# ::DISPATCH( $GLOBAL::Code_print, 'APPLY', ::DISPATCH( $MATCH, 'perl', ) );
ok( $MATCH->true, " matched" );
print "# ",$MATCH->result,"\n";
print "# ",($MATCH->from),"\n";
print "# ",($MATCH->to),"\n";
ok( $MATCH->Str eq '123', " Str" );


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
