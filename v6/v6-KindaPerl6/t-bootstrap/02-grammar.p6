
# how to run this program before bootstrapping:
#
# $ perl script/kp6 -Cperl5rx t-bootstrap/02-grammar.p6 | perl -I compiled/perl5-kp6-kp6/lib
#

use KindaPerl6::Runtime::Perl5::Runtime;
use KindaPerl6::Grammar::Quote;
use KindaPerl6::Grammar;
use KindaPerl6::Ast;

say "1..4";
my $count=0;
sub ok($ok,$desc) {
    $count = $count + 1;
    if ($ok) {
        say "ok $count - $desc";
    } else {
        say "not ok $count - $desc";
    }
}

# digits

say "# ** now testing: <digits>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.digits();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq '123','digits');

# val_int

say "# ** now testing: <val_int>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.val_int();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Val::Int.new(int => '123')",'val_int');

# simple term

say "# ** now testing: <term>";
$_ = '...';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [  ], code => Var.new(namespace => [  ], name => 'die', twigil => '', sigil => '&'))",'term ...');

# simple term

say "# ** now testing: <term>";
$_ = 'Inf';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Apply.new(arguments => [  ], code => Var.new(namespace => [  ], name => 'Inf', twigil => '', sigil => '&'))",'term Inf');;


# $var 

say "# ** now testing: <var>";
$_ = '$var';
my $MATCH = KindaPerl6::Grammar.var();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Var.new(namespace => [  ], name => 'var', twigil => '', sigil => '$'))",'$var');;


# $var term

say "# ** now testing: <term> with \$var";
$_ = '$var';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq "Var.new(namespace => [  ], name => 'var', twigil => '', sigil => '$'))",'$var term');


#term

say "# ** now testing: <term>";
$_ = '123';
my $MATCH = KindaPerl6::Grammar.term();
say "# ",($MATCH.perl);
say "# ",($MATCH.Str);
ok( $MATCH.Str eq '123','term');



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
