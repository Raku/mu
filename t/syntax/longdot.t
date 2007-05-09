use v6-alpha;

use Test;

plan 53;

# L<S02/"Whitespace and Comments"/"long dot">

#Test the "long dot" and unspace syntax

class Str is also {
    method id($x:) { $x }
}

#This makes 'foo.id' and 'foo .id' mean different things
multi foo() { 'a' }
multi foo($x) { $x }

#This should do the same, but currently doesn't
sub bar($x? = 'a') { $x }

$_ = 'b';

#XXX why is eval required here?
is(eval('foo.id'), 'a', 'sanity - foo.id');
is(eval('foo .id'), 'b', 'sanity - foo .id');
is(eval('bar.id'), 'a', 'sanity - bar.id');
is(eval('bar .id'), 'b', 'sanity - bar .id');
is(eval('foo\.id'), 'a', 'short long dot');
is(eval('foo\ .id'), 'a', 'long dot');
is(eval('foo \ .id'), 'b', 'not a long dot');
is(eval('fo\ o.id'), undef, 'unspace not allowed in identifier');
is(eval('foo\    .id'), 'a', 'longer dot');
is(eval('foo\#( comment ).id'), 'a', 'long dot with embedded comment');
is(eval('foo\#\ ( comment ).id'), undef, 'unspace can\'t hide space between # and opening bracket');
is(eval('foo\ # comment
    .id'), 'a', 'long dot with end-of-line comment');
is(eval('foo\
=begin comment
blah blah blah
=end comment
    .id'), 'a', 'long dot with pod =begin/=end comment');
is(eval('foo\
=for comment
blah
blah
blah

    .id'), 'a', 'long dot with pod =for comment');
is(eval('foo\
=comment blah blah blah
    .id'), 'a', 'long dot with pod =comment');
# L<S02/"Whitespace and Comments"/"natural conflict between postfix operators and infix operators">
my $n = 1;
my $m = 2;
sub infix:<++>($x, $y) { 42 }
is(eval('$n++$m'), undef, 'infix requires space when ambiguous with postfix');
#no, really: http://moritz.faui2k3.org/irclog/out.pl?channel=perl6;date=2007-05-09#id_l328
is(eval('$n ++$m'), 42, '$n ++$m with infix:<++> is $n ++ $m');
is(eval('$n ++ $m'), 42, 'postfix requires no space w/ infix ambiguity');
is(eval('$n.++ $m'), undef, 'postfix dot w/ infix ambiguity');
is(eval('$n\ ++ $m'), undef, 'postfix unspace w/ infix ambiguity');
is(eval('$n\ .++ $m'), undef, 'postfix long dot w/ infix ambiguity');
$n = 1;
is(eval('$n ++'), undef, 'postfix requires no space');
$n = 1;
is(eval('$n.++'), 2, 'postfix dot');
$n = 1;
is(eval('$n\ ++'), 2, 'postfix unspace');
$n = 1;
is(eval('$n\ .++'), 2, 'postfix long dot');
# L<S02/"Lexical Conventions"/"U+301D has two closing alternatives">
is(eval('foo\#〝 comment 〞.id'), 'a', 'long dot with U+301D/U+301E comment');
is(eval('foo\#〝 comment 〟.id'), undef, 'long dot with U+301D/U+301F is invalid');
#Unicode whitespace
# L<S02/"Lexical Conventions"/"Unicode horizontal whitespace">
is(eval('foo\	.id'), 'a', 'long dot with CHARACTER TABULATION');
is(eval('foo\
.id'), 'a', 'long dot with LINE FEED (LF)');
is(eval('foo\.id'), 'a', 'long dot with LINE TABULATION');
is(eval('foo\.id'), 'a', 'long dot with FORM FEED (FF)');
is(eval('foo\.id'), 'a', 'long dot with CARRIAGE RETURN (CR)');
is(eval('foo\ .id'), 'a', 'long dot with SPACE');
is(eval('foo\.id'), 'a', 'long dot with NEXT LINE (NEL)');
is(eval('foo\ .id'), 'a', 'long dot with NO-BREAK SPACE');
is(eval('foo\ .id'), 'a', 'long dot with OGHAM SPACE MARK');
is(eval('foo\᠎.id'), 'a', 'long dot with MONGOLIAN VOWEL SEPARATOR');
is(eval('foo\ .id'), 'a', 'long dot with EN QUAD');
is(eval('foo\ .id'), 'a', 'long dot with EM QUAD');
is(eval('foo\ .id'), 'a', 'long dot with EN SPACE');
is(eval('foo\ .id'), 'a', 'long dot with EM SPACE');
is(eval('foo\ .id'), 'a', 'long dot with THREE-PER-EM SPACE');
is(eval('foo\ .id'), 'a', 'long dot with FOUR-PER-EM SPACE');
is(eval('foo\ .id'), 'a', 'long dot with SIX-PER-EM SPACE');
is(eval('foo\ .id'), 'a', 'long dot with FIGURE SPACE');
is(eval('foo\ .id'), 'a', 'long dot with PUNCTUATION SPACE');
is(eval('foo\ .id'), 'a', 'long dot with THIN SPACE');
is(eval('foo\ .id'), 'a', 'long dot with HAIR SPACE');
is(eval('foo\ .id'), 'a', 'long dot with LINE SEPARATOR');
is(eval('foo\ .id'), 'a', 'long dot with PARAGRAPH SEPARATOR');
is(eval('foo\ .id'), 'a', 'long dot with NARROW NO-BREAK SPACE');
is(eval('foo\ .id'), 'a', 'long dot with MEDIUM MATHEMATICAL SPACE');
is(eval('foo\　.id'), 'a', 'long dot with IDEOGRAPHIC SPACE');
