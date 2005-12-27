use Test;
use Parse::Rule::Strategies::CPS::Text;

plan 77;

my $c = Parse::Rule::Strategies::CPS::Text.new;

sub do_match ($text, $parser) {
    my $rule = $parser.compile;
    my ($match, $backtrack) = $rule.run(
        input => $c.Pos.new(text => $text, pos => 0),
        match => Parse::Rule::Core::Match.new,
    );
    $match;
}

my ($pat, $desc);

sub matches ($text) {
    my $m = do_match($text, $pat);
    my $end;
    my $matches = defined $m and 
                    do { $end = $m.to; $end.pos == $text.chars };
    ok($matches, "'$text' matches $desc");
    return $m;
}

sub matches_not ($text) {
    my $m = do_match($text, $pat);
    my $end;
    my $matches = defined $m and 
                    do { $end = $m.to; $end.pos == $text.chars };
    ok(!$matches, "'$text' doesn't match $desc");
}

sub is_range ($match, $start, $end) {
    my ($ms, $me) = ($match.from, $match.to);
    my ($msp, $mep) = ($ms.pos, $me.pos);
    
    ok($msp == $start && $mep == $end, "capture ($msp, $mep) is ($start, $end)");
}


($desc, $pat) = ('/<null>/', $c.empty());
    matches "";
    matches_not "x";

($desc, $pat) = ('/hello/', $c.literal("hello"));
    matches "hello";
    matches_not "goodbye";
    matches_not "hell";
    matches_not "helloes";

($desc, $pat) = ('/./', $c.any_char());
    matches "x";
    matches_not "";
    matches_not "xx";

($desc, $pat) = ('/foo bar/', $c.concat($c.literal("foo"), $c.literal("bar")));
    matches "foobar";
    matches_not "foo";
    matches_not "bar";
    matches_not "";

($desc, $pat) = ('/foo|bar/', $c.alternate($c.literal("foo"), $c.literal("bar")));
    matches "foo";
    matches "bar";
    matches_not "foobar";
    matches_not "";

($desc, $pat) = ('/[foo|bar] [baz|quux]/',
                    $c.concat(
                        $c.alternate($c.literal("foo"), $c.literal("bar")),
                        $c.alternate($c.literal("baz"), $c.literal("quux"))));
    matches "foobaz";
    matches "fooquux";
    matches "barbaz";
    matches "barquux";
    matches_not "foo";
    matches_not "";
    matches_not "quux";
    matches_not "foobarbaz";

($desc, $pat) = ('/x*/', $c.quantify($c.literal("x")));
    matches "";
    matches "x";
    matches "xxxxxx";
    matches_not "xxxxxxy";
    matches_not "yxxxxxx";

($desc, $pat) = ('/x+/', $c.quantify($c.literal("x"), 1));
    matches "x";
    matches "xxxxxx";
    matches_not "";
    matches_not "y";

($desc, $pat) = ('/[x|y]*/', $c.quantify($c.alternate($c.literal("x"), $c.literal("y"))));
    matches "";
    matches "x";
    matches "y";
    matches_not "z";
    matches "xxyxyyyxyyyxyx";
    matches_not "xxyyyxyxz";

my $match;

($desc, $pat) = ('/(x)/', $c.capture($c.literal("x"), :num(0)));
    $match = matches "x";
    is_range($match.capture_num[0], 0, 1);
    matches_not "";
    matches_not "xx";

($desc, $pat) = ('/xy(zz)/', $c.concat($c.literal("xy"), $c.capture($c.literal("zz"), :num(0))));
    $match = matches "xyzz";
    is_range($match.capture_num[0], 2, 4);

($desc, $pat) = ('/(xy) $zs := (zz)/', $c.concat($c.capture($c.literal("xy"), :num(0)), 
                                              $c.capture($c.literal("zz"), :num(1), :name<zs>)));
    $match = matches "xyzz";
    is_range($match.capture_num[0], 0, 2);
    is_range($match.capture_num[1], 2, 4);
    is_range($match.capture_name<zs>, 2, 4);

($desc, $pat) = ('/(x|y)*/', $c.quantify($c.capture($c.alternate($c.literal("x"), $c.literal("y")), :num(0))));
    $match = matches "xyyx";
    is_range($match.capture_num[0][0], 0, 1);
    is_range($match.capture_num[0][1], 1, 2);
    is_range($match.capture_num[0][2], 2, 3);
    is_range($match.capture_num[0][3], 3, 4);

($desc, $pat) = ('/[ (foo|bar) $bq:=(baz|quux) ]+/', 
                    $c.quantify( :low(1),
                        $c.concat($c.capture($c.alternate($c.literal("foo"), $c.literal("bar")), :num(0)),
                               $c.capture($c.alternate($c.literal("baz"), $c.literal("quux")), :num(1), :name<bq>))));
    matches_not "";
    matches_not "foobazbar";
    $match = matches "foobazbarquuxbarbaz";
    is_range($match.capture_num[0][0], 0,3);
    is_range($match.capture_num[0][1], 6,9);
    is_range($match.capture_num[0][2], 13,16);
    is_range($match.capture_num[1][0], 3,6);
    is_range($match.capture_num[1][1], 9,13);
    is_range($match.capture_num[1][2], 16,19);
    is_range($match.capture_name<bq>[0], 3,6);
    is_range($match.capture_name<bq>[1], 9,13);
    is_range($match.capture_name<bq>[2], 16,19);

($desc, $pat) = ('/[ [ x ]* ]*/', $c.quantify($c.quantify($c.literal("x"))));
    matches "x";
    matches_not "xy";

($desc, $pat) = ('/"(.*)".*/', 
                    $c.concat(
                        $c.concat(
                            $c.concat(
                                $c.literal('"'),
                                $c.capture(:num(0),
                                    $c.quantify($c.any_char()))),
                            $c.literal('"')),
                        $c.quantify($c.any_char())));
    $match = matches q{"foobar"baz"quux"ziph};
    is_range($match.capture_num[0], 1, 16);

($desc, $pat) = ('/"(.*?)".*/', 
                    $c.concat(
                        $c.concat(
                            $c.concat(
                                $c.literal('"'),
                                $c.capture(:num(0),
                                    $c.quantify(:minimal, $c.any_char()))),
                            $c.literal('"')),
                        $c.quantify($c.any_char())));
    $match = matches q{"foobar"baz"quux"ziph};
    is_range($match.capture_num[0], 1, 7);

($desc, $pat) = ('/ x*:x /', 
                 $c.concat(
                    $c.mark(:name(':'), $c.concat($c.quantify($c.literal("x")), $c.commit(':'))), 
                    $c.literal("x")));
    matches_not "xxx";

($desc, $pat) = ('/ yx?y /', 
                $c.concat(
                    $c.concat($c.literal('y'), $c.optional($c.literal('x'))),
                    $c.literal('y')));
    matches 'yxy';
    matches 'yy';
    matches_not 'y';

# vim: ft=perl6 :
