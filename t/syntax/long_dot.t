use v6-alpha;

use Test;

plan 12;

is(4\       .sqrt, 2, 'long dot with numbers');
is(4\#(quux).sqrt, 2, 'long dot with comments');
is("x"\     .bytes, 1, 'long dot with strings');
is("x"\     .bytes(), 1, 'long dot with strings + parens');

my $foo = 4;
is($foo.++, 5, '(short) long dot with postfix inc');
is($foo\       .++, 6, 'long dot with postfix inc');
is($foo\       .--, 5, 'long dot with postfix dec');

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
