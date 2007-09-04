use v6-alpha;

use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/anchors.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

L<S05/New metacharacters/"^^ and $$ match line beginnings and endings">

=cut

plan 19;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $str = q{abc
def
ghi};

ok(   $str ~~ m/^abc/, 'SOS abc' );
ok(!( $str ~~ m/^bc/ ), 'SOS bc' );
ok(   $str ~~ m/^^abc/, 'SOL abc' );
ok(!( $str ~~ m/^^bc/ ), 'SOL bc' );
ok(   $str ~~ m/abc\n?$$/, 'abc newline EOL' );
ok(   $str ~~ m/abc$$/, 'abc EOL' );
ok(!( $str ~~ m/ab$$/ ), 'ab EOL' );
ok(eval(' !( $str ~~ m/abc$/ ) '), 'abc EOS' );
ok(eval(' !( $str ~~ m/^def/ ) '), 'SOS def' );
ok(eval('    $str ~~ m/^^def/ '), 'SOL def' );
ok(eval('    $str ~~ m/def\n?$$/ '), 'def newline EOL' );
ok(eval('    $str ~~ m/def$$/ '), 'def EOL' );
ok(eval(' !( $str ~~ m/def$/ ) '), 'def EOS' );
ok(eval(' !( $str ~~ m/^ghi/ ) '), 'SOS ghi' );
ok(eval('    $str ~~ m/^^ghi/ '), 'SOL ghi' );
ok(eval('    $str ~~ m/ghi\n?$$/ '), 'ghi newline EOL' );
ok(eval('    $str ~~ m/ghi$$/ '), 'ghi EOL' );
ok(eval('    $str ~~ m/ghi$/ '), 'ghi EOS' );
ok(eval('    $str ~~ m/^abc$$\n^^d.*f$$\n^^ghi$/ '), 'All dot' );

}
