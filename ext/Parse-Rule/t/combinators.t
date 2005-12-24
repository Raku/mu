use Test;
use Parse::Rule::Core;
use Parse::Rule::Media;
use Parse::Rule::Combinators;

plan 70;

sub do_match ($text, $parser) {
    $parser.parse()(
        Result.new(
            backtrack => -> { undef },
            pos       => Text.new(text => $text, pos => 0),
            match     => Match.new,
        ),
        -> $m { $m },
    );
}

say mark(undef, ':');

my ($pat, $desc);

sub matches ($text) {
    my $m = do_match($text, $pat);
    my $pos;
    my $matches = defined $m and 
                    do { $pos = $m.pos; $pos.pos == $text.chars };
    ok($matches, "'$text' matches $desc");
    return $m.match;
}

sub matches_not ($text) {
    my $m = do_match($text, $pat);
    my $pos;
    my $matches = defined $m and 
                    do { $pos = $m.pos; $pos.pos == $text.chars };
    ok(!$matches, "'$text' doesn't match $desc");
}

sub is_range ($match, $start, $end) {
    my ($ms, $me) = ($match.start, $match.end);
    my ($msp, $mep) = ($ms.pos, $me.pos);
    
    ok($msp == $start && $mep == $end, "capture ($msp, $mep) is ($start, $end)");
}


($desc, $pat) = ('/<null>/', empty());
    matches "";
    matches_not "x";

($desc, $pat) = ('/hello/', Text::literal("hello"));
    matches "hello";
    matches_not "goodbye";
    matches_not "hell";
    matches_not "helloes";

($desc, $pat) = ('/./', Text::any_char());
    matches "x";
    matches_not "";
    matches_not "xx";

($desc, $pat) = ('/foo bar/', concat(Text::literal("foo"), Text::literal("bar")));
    matches "foobar";
    matches_not "foo";
    matches_not "bar";
    matches_not "";

($desc, $pat) = ('/foo|bar/', alternate(Text::literal("foo"), Text::literal("bar")));
    matches "foo";
    matches "bar";
    matches_not "foobar";
    matches_not "";

($desc, $pat) = ('/[foo|bar] [baz|quux]/',
                    concat(
                        alternate(Text::literal("foo"), Text::literal("bar")),
                        alternate(Text::literal("baz"), Text::literal("quux"))));
    matches "foobaz";
    matches "fooquux";
    matches "barbaz";
    matches "barquux";
    matches_not "foo";
    matches_not "";
    matches_not "quux";
    matches_not "foobarbaz";

($desc, $pat) = ('/x*/', quantify(Text::literal("x")));
    matches "";
    matches "x";
    matches "xxxxxx";
    matches_not "xxxxxxy";
    matches_not "yxxxxxx";

($desc, $pat) = ('/x+/', quantify(Text::literal("x"), 1));
    matches "x";
    matches "xxxxxx";
    matches_not "";
    matches_not "y";

($desc, $pat) = ('/[x|y]*/', quantify(alternate(Text::literal("x"), Text::literal("y"))));
    matches "";
    matches "x";
    matches "y";
    matches_not "z";
    matches "xxyxyyyxyyyxyx";
    matches_not "xxyyyxyxz";

my $match;

($desc, $pat) = ('/(x)/', capture(Text::literal("x"), :num(0)));
    $match = matches "x";
    is_range($match.match_num[0], 0, 1);
    matches_not "";
    matches_not "xx";

($desc, $pat) = ('/xy(zz)/', concat(Text::literal("xy"), capture(Text::literal("zz"), :num(0))));
    $match = matches "xyzz";
    is_range($match.match_num[0], 2, 4);

($desc, $pat) = ('/(xy) $zs := (zz)/', concat(capture(Text::literal("xy"), :num(0)), 
                                              capture(Text::literal("zz"), :num(1), :name<zs>)));
    $match = matches "xyzz";
    is_range($match.match_num[0], 0, 2);
    is_range($match.match_num[1], 2, 4);
    is_range($match.match_name<zs>, 2, 4);

($desc, $pat) = ('/(x|y)*/', quantify(capture(alternate(Text::literal("x"), Text::literal("y")), :num(0))));
    $match = matches "xyyx";
    is_range($match.match_num[0][0], 0, 1);
    is_range($match.match_num[0][1], 1, 2);
    is_range($match.match_num[0][2], 2, 3);
    is_range($match.match_num[0][3], 3, 4);

($desc, $pat) = ('/[ (foo|bar) $bq:=(baz|quux) ]+/', 
                    quantify( :low(1),
                        concat(capture(alternate(Text::literal("foo"), Text::literal("bar")), :num(0)),
                               capture(alternate(Text::literal("baz"), Text::literal("quux")), :num(1), :name<bq>))));
    matches_not "";
    matches_not "foobazbar";
    $match = matches "foobazbarquuxbarbaz";
    is_range($match.match_num[0][0], 0,3);
    is_range($match.match_num[0][1], 6,9);
    is_range($match.match_num[0][2], 13,16);
    is_range($match.match_num[1][0], 3,6);
    is_range($match.match_num[1][1], 9,13);
    is_range($match.match_num[1][2], 16,19);
    is_range($match.match_name<bq>[0], 3,6);
    is_range($match.match_name<bq>[1], 9,13);
    is_range($match.match_name<bq>[2], 16,19);

($desc, $pat) = ('/[ [ x ]* ]*/', quantify(quantify(Text::literal("x"))));
    matches "x";
    matches_not "xy";

($desc, $pat) = ('/ x*:x /', 
                 concat(
                    mark(:name(':'), concat(quantify(Text::literal("x")), commit(':'))), 
                    Text::literal("x")));
    matches_not "xxx";

# vim: ft=perl6 :
