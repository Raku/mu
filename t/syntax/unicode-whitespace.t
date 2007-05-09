use v6-alpha;

use Test;

plan 26;

# L<S02/"Lexical Conventions"/"Unicode horizontal whitespace">

is(eval('
my	@x	=	<a	b	c>;	sub	y	(@z)	{	@z[1]	};	y(@x)
'), "b", "CHARACTER TABULATION");

is(eval('
my
@x
=
<a
b
c>;
sub
y
(@z)
{
@z[1]
};
y(@x)
'), "b", "LINE FEED (LF)");

is(eval('
my@x=<abc>;suby(@z){@z[1]};y(@x)
'), "b", "LINE TABULATION");

is(eval('
my@x=<abc>;suby(@z){@z[1]};y(@x)
'), "b", "FORM FEED (FF)");

is(eval('
my@x=<abc>;suby(@z){@z[1]};y(@x)
'), "b", "CARRIAGE RETURN (CR)");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "SPACE");

is(eval('
my@x=<abc>;suby(@z){@z[1]};y(@x)
'), "b", "NEXT LINE (NEL)");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "NO-BREAK SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "OGHAM SPACE MARK");

is(eval('
my᠎@x᠎=᠎<a᠎b᠎c>;᠎sub᠎y᠎(@z)᠎{᠎@z[1]᠎};᠎y(@x)
'), "b", "MONGOLIAN VOWEL SEPARATOR");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "EN QUAD");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "EM QUAD");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "EN SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "EM SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "THREE-PER-EM SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "FOUR-PER-EM SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "SIX-PER-EM SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "FIGURE SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "PUNCTUATION SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "THIN SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "HAIR SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "LINE SEPARATOR");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "PARAGRAPH SEPARATOR");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "NARROW NO-BREAK SPACE");

is(eval('
my @x = <a b c>; sub y (@z) { @z[1] }; y(@x)
'), "b", "MEDIUM MATHEMATICAL SPACE");

is(eval('
my　@x　=　<a　b　c>;　sub　y　(@z)　{　@z[1]　};　y(@x)
'), "b", "IDEOGRAPHIC SPACE");
