use Test;
use Parse::Rule::Core;
use Parse::Rule::Media;
use Parse::Rule::Combinators;

plan 9;

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

sub matches ($text, $rx, $parser) {
    my $m = do_match($text, $parser);
    my $pos;
    my $matches = defined $m and 
                    do { $pos = $m.pos; $pos.pos == $text.chars };
    ok($matches, "'$text' matches /$rx/");
}

sub matches_not ($text, $rx, $parser) {
    my $m = do_match($text, $parser);
    my $pos;
    my $matches = defined $m and 
                    do { $pos = $m.pos; $pos.pos == $text.chars };
    ok(!$matches, "'$text' doesn't match $rx");
}

matches("", '/<null>/', empty());
matches_not("x", '/<null>/', empty());

matches("hello",       '/hello/', Text::literal("hello"));
matches_not("goodbye", '/hello/', Text::literal("hello"));
matches_not("hell",    '/hello/', Text::literal("hello"));
matches_not("helloes", '/hello/', Text::literal("hello"));

matches("x",      '/./', Text::any_char());
matches_not("",   '/./', Text::any_char());
matches_not("xx", '/./', Text::any_char());

# vim: ft=perl6 :
