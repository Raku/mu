use v6-alpha;
# vim: filetype=perl6 :

# NOTE on porting quotemeta.t from Perl 5.9.3: the original test suite
# (see the pod-commented out section at the end of this file)
# did include may tests to exercise the behaviour in double-quotes
# interpolation with \Q and \E, and their interaction with other
# modification like \L and \U. These interpolating sequences no longer
# exist.

use Test;

plan 10;

# L<S29/"Perl6::Str" /quotemeta/>

is(quotemeta("HeLLo World-72_1"), "HeLLo\\ World\\-72_1", "simple lc test");
is(quotemeta(""), "", "empty string");

$_ = "HeLLo World-72_1"; 
my $x = .quotemeta;
is($x, "HeLLo\\ World\\-72_1", 'quotemeta uses $_ as default');

{ # test invocant syntax for quotemeta
    my $x = "HeLLo World-72_1";
    is($x.quotemeta, "HeLLo\\ World\\-72_1", '$x.quotemeta works');
    is("HeLLo World-72_1".quotemeta, "HeLLo\\ World\\-72_1", '"HeLLo World-72_1".quotemeta works');
}


if (%*Config<ebcdic> eq 'define') {
    $_ = (129 .. 233).map({ chr($_); }).join('');
    is($_.chars, 96, "quotemeta starting string");
    
    # 105 characters - 52 letters = 53 backslashes
    # 105 characters + 53 backslashes = 158 characters
    $_ = quotemeta $_;
    is($_.chars, 158, "quotemeta string");
    # 53 backslashed characters + 1 "original" backslash
    is($_.split('').grep({ $_ eq "\x5c" }).elems, 54, "count backslashes");
}
else {
    $_ = (32 .. 127).map({ chr($_); }).join('');
    is($_.chars, 96, "quotemeta starting string");
    
    # 96 characters - 52 letters - 10 digits - 1 underscore = 33 backslashes
    # 96 characters + 33 backslashes = 129 characters
    $_ = quotemeta $_;
    is($_.chars, 129, "quotemeta string");
    # 33 backslashed characters + 1 "original" backslash
    is($_.split('').grep({ $_ eq "\x5c" }).elems, 34, "count backslashes");
}

# I don't exactly know why the following tests should apply.
# The only thing I suspect is that quotemeta in Perl5:
#
# * ignores Unicode and treats strings as sequences of octets
# * does not escape characters that are outside the ASCII range 32 .. 127
#   or the EBCDIC range 129 .. 233
#
# This is indirectly confirmed by the tests above (they focus on these
# ranges only) and by the fact that Unicode character \x[263a] corresponds
# to the UTF-8 sequence [0xe2, 0x98, 0xba], in which each octet is outside
# the given ranges.
# 
# Should we stick to this?
is(quotemeta("\x[263a]"), "\x[263a]", "quotemeta Unicode", :todo<bug>);
is(quotemeta("\x[263a]").chars, 1, "quotemeta Unicode length", :todo<bug>);

=begin from_perl5


plan tests => 22;

if ($Config{ebcdic} eq 'define') {
    $_ = join "", map chr($_), 129..233;

    # 105 characters - 52 letters = 53 backslashes
    # 105 characters + 53 backslashes = 158 characters
    $_ = quotemeta $_;
    is(length($_), 158, "quotemeta string");
    # 104 non-backslash characters
    is(tr/\\//cd, 104, "tr count non-backslashed");
} else { # some ASCII descendant, then.
    $_ = join "", map chr($_), 32..127;

    # 96 characters - 52 letters - 10 digits - 1 underscore = 33 backslashes
    # 96 characters + 33 backslashes = 129 characters
    $_ = quotemeta $_;
    is(length($_), 129, "quotemeta string");
    # 95 non-backslash characters
    is(tr/\\//cd, 95, "tr count non-backslashed");
}

is(length(quotemeta ""), 0, "quotemeta empty string");

is("aA\UbB\LcC\EdD", "aABBccdD", 'aA\UbB\LcC\EdD');
is("aA\LbB\UcC\EdD", "aAbbCCdD", 'aA\LbB\UcC\EdD');
is("\L\upERL", "Perl", '\L\upERL');
is("\u\LpERL", "Perl", '\u\LpERL');
is("\U\lPerl", "pERL", '\U\lPerl');
is("\l\UPerl", "pERL", '\l\UPerl');
is("\u\LpE\Q#X#\ER\EL", "Pe\\#x\\#rL", '\u\LpE\Q#X#\ER\EL');
is("\l\UPe\Q!x!\Er\El", "pE\\!X\\!Rl", '\l\UPe\Q!x!\Er\El');
is("\Q\u\LpE.X.R\EL\E.", "Pe\\.x\\.rL.", '\Q\u\LpE.X.R\EL\E.');
is("\Q\l\UPe*x*r\El\E*", "pE\\*X\\*Rl*", '\Q\l\UPe*x*r\El\E*');
is("\U\lPerl\E\E\E\E", "pERL", '\U\lPerl\E\E\E\E');
is("\l\UPerl\E\E\E\E", "pERL", '\l\UPerl\E\E\E\E');

is(quotemeta("\x{263a}"), "\x{263a}", "quotemeta Unicode");
is(length(quotemeta("\x{263a}")), 1, "quotemeta Unicode length");

$a = "foo|bar";
is("a\Q\Ec$a", "acfoo|bar", '\Q\E');
is("a\L\Ec$a", "acfoo|bar", '\L\E');
is("a\l\Ec$a", "acfoo|bar", '\l\E');
is("a\U\Ec$a", "acfoo|bar", '\U\E');
is("a\u\Ec$a", "acfoo|bar", '\u\E');

=end from_perl5

=cut
