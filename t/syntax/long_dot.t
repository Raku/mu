use v6-alpha;

use Test;

plan 74;

# L<S02/"Whitespace and Comments"/"long dot">

is(4\       .sqrt, 2, 'long dot with numbers');
is(4\#(quux).sqrt, 2, 'long dot with comments');
is("x"\     .bytes, 1, 'long dot with strings');
is("x"\     .bytes(), 1, 'long dot with strings + parens');

my $foo = 4;
is(eval('$foo.++'), 4, '(short) long dot with postfix inc');
is($foo, 5, '(short) long dot with postfix inc really postfix');
is(eval('$foo\       .++'), 5, 'long dot with postfix inc');
is($foo, 6, 'long dot with postfix inc really postfix');
is(eval('$foo\       .--'), 6, 'long dot with postfix dec');
is($foo, 5, 'long dot with postfix dec really postfix');

is("xxxxxx"\.bytes, 6, 'long dot without spaces');
is("xxxxxx"\
    .bytes, 6, 'long dot with newline');

is((:foo\ .("bar")), ('foo' => "bar"), 'long dot with adverb');

is( ~([1,2,3]\ .[2,1,0]), "3 2 1", 'long dot on postfix subscript');

my @array = 1,2,3;

eval "
    @array\    .>>++;
    @array>>\    .++;
    @array\ .>>\ .++;
    @array\     .»++;
    @array»\     .++;
    @array\ .»\  .++;
";
is( ~@array, "7 8 9", 'long dots with postfix hyperops', :todo);


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
is(eval(':foo\ <bar>'), (:foo<bar>), 'unspace in colonpair');
is(eval('foo\ .\ ("x")'), 'x', 'unspace is allowed both before and after method .');
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
#This is pretty strange: according to Perl-6.0.0-STD.pm,
#unspace is allowed after a pod = ... which means pod is
#syntactically recursive, i.e. you can put pod comments
#inside pod directives recursively!
is(eval('foo\
=\ begin comment
blah blah blah
=\ end comment
    .id'), 'a', 'long dot with pod =begin/=end comment w/ pod unspace');
is(eval('foo\
=\ for comment
blah
blah
blah

    .id'), 'a', 'long dot with pod =for comment w/ pod unspace');
is(eval('foo\
=\ comment blah blah blah
    .id'), 'a', 'long dot with pod =comment w/ pod unspace');
is(eval('foo\
=\
=begin nested pod
blah blah blah
=end nested pod
begin comment
blah blah blah
=\
=begin nested pod
blah blah blah
=end nested pod
end comment
    .id'), 'a', 'long dot with pod =begin/=end comment w/ pod-in-pod');
is(eval('foo\
=\
=for nested pod
blah
blah
blah

for comment
blah
blah
blah

    .id'), 'a', 'long dot with pod =for commenti w/ pod-in-pod');
is(eval('foo\
=\
=nested pod blah blah blah
comment blah blah blah
    .id'), 'a', 'long dot with pod =comment w/ pod-in-pod');
is(eval('foo\
=\			#1
=\			#2
=\			#3
=comment blah blah blah
for comment		#3
blah
blah
blah

begin comment		#2
blah blah blah
=\			#4
=comment blah blah blah
end comment		#4
begin comment		#1
blah blah blah
=\			#5
=\			#6
=for comment
blah
blah
blah

comment blah blah blah	#6
end comment		#5
    .id'), 'a', 'hideous nested pod torture test');

# L<S04/"Statement-ending blocks"/"Because subroutine declarations are expressions">
#XXX probably shouldn't be in this file...

eval('sub f { 3 } sub g { 3 }');
is(eval('f'), undef, 'semicolon or newline required between blocks');
is(eval('g'), undef, 'semicolon or newline required between blocks');

# L<S06/"Blocks"/"unless followed immediately by a comma">

sub baz(Code $x, *@y) { $x.(@y) }

is(eval('baz { @^x }, 1, 2, 3'), (1, 2, 3), 'comma immediately following arg block');
is(eval('baz { @^x } , 1, 2, 3'), undef, 'comma not immediately following arg block');
is(eval('baz { @^x }\ , 1, 2, 3'), (1, 2, 3), 'unspace then comma following arg block');

class Code is also {
    method xyzzy(Code $x: *@y) { $x.(@y) }
}

is(eval('xyzzy { @^x }: 1, 2, 3'), (1, 2, 3), 'colon immediately following arg block');
is(eval('xyzzy { @^x } : 1, 2, 3'), undef, 'colon not immediately following arg block');
is(eval('xyzzy { @^x }\ : 1, 2, 3'), (1, 2, 3), 'unspace then colon following arg block');

# L<S02/"Whitespace and Comments"/"natural conflict between postfix operators and infix operators">
#This creates syntactic ambuguity between
#($n) ++ ($m)
#($n++) $m
#($n) (++$m)
#($n) + (+$m)

my $n = 1;
my $m = 2;
sub infix:<++>($x, $y) { 42 }

#'$n++$m' should be a syntax error
is(eval('$n++$m'), undef, 'infix requires space when ambiguous with postfix');
is($n, 1, 'check $n');
is($m, 2, 'check $m');

#'$n ++$m' should be infix:<++>
#no, really: http://moritz.faui2k3.org/irclog/out.pl?channel=perl6;date=2007-05-09#id_l328
$n = 1; $m = 2;
is(eval('$n ++$m'), 42, '$n ++$m with infix:<++> is $n ++ $m');
is($n, 1, 'check $n');
is($m, 2, 'check $m');

#'$n ++ $m' should be infix:<++>
$n = 1; $m = 2;
is(eval('$n ++ $m'), 42, 'postfix requires no space w/ infix ambiguity');
is($n, 1, 'check $n');
is($m, 2, 'check $m');

#These should all be postfix syntax errors
$n = 1; $m = 2;
is(eval('$n.++ $m'), undef, 'postfix dot w/ infix ambiguity');
is(eval('$n\ ++ $m'), undef, 'postfix unspace w/ infix ambiguity');
is(eval('$n\ .++ $m'), undef, 'postfix long dot w/ infix ambiguity');
is($n, 1, 'check $n');
is($m, 2, 'check $m');

#Unspace inside operator splits it
$n = 1; $m = 2;
is(eval('$n+\ +$m'), 3, 'unspace inside operator splits it');
is($n, 1, 'check $n');
is($m, 2, 'check $m');

$n = 1;
is(eval('$n ++'), undef, 'postfix requires no space');
is($n, 1, 'check $n');

$n = 1;
is(eval('$n.++'), 1, 'postfix dot');
is($n, 2, 'check $n');

$n = 1;
is(eval('$n\ ++'), 1, 'postfix unspace');
is($n, 2, 'check $n');

$n = 1;
is(eval('$n\ .++'), 1, 'postfix long dot');
is($n, 2, 'check $n');

# L<S02/"Lexical Conventions"/"U+301D has two closing alternatives">
is(eval('foo\#〝 comment 〞.id'), 'a', 'long dot with U+301D/U+301E comment');
is(eval('foo\#〝 comment 〟.id'), undef, 'long dot with U+301D/U+301F is invalid');
