
use lib "compiled/perl5-kp6-kp6/lib";
use strict;
use KindaPerl6::Runtime::Perl5::Runtime;
use KindaPerl6::Grammar::Quote;
use Test::More tests => 3;

$_ = ::DISPATCH( $::Scalar, "new" );
my $MATCH;

::DISPATCH_VAR( $_, 'STORE', ::DISPATCH( $::Str, 'new', '123' ) );
$MATCH = ::DISPATCH( $::KindaPerl6::Grammar, 'quoted_any' );
# ::DISPATCH( $GLOBAL::Code_print, 'APPLY', ::DISPATCH( $MATCH, 'perl', ) );
ok( $MATCH->true, "quoted_any matched" );

::DISPATCH_VAR( $_, 'STORE', ::DISPATCH( $::Str, 'new', '123"' ) );
$MATCH = ::DISPATCH( $::KindaPerl6::Grammar, 'double_quoted' );
# ::DISPATCH( $GLOBAL::Code_print, 'APPLY', ::DISPATCH( $MATCH, 'perl', ) );
ok( $MATCH->true, "double_quoted matched" );
ok( $MATCH->Str eq '123', "double_quoted Str" );


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
