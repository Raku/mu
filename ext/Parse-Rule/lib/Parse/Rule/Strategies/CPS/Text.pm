class Parse::Rule::Strategies::CPS::Text;

use Parse::Rule::Media::Text;
does Parse::Rule::Media::Text;

use Parse::Rule::Strategies::CPS;
does Parse::Rule::Strategies::CPS;

method literal (Str $text) {
    $.Parser.new(parse => sub ($match, &continue) {
        my $pos = $match.pos;
        if $pos.text.substr($pos.pos, $text.chars) eq $text {
            &continue($match.clone(
                pos => $pos.clone(pos => $pos.pos + $text.chars),
            ));
        }
        else {
            $match.backtrack()();
        }
    });
}

method any_char () {
    $.Parser.new(parse => sub ($match, &continue) {
        my $pos = $match.pos;
        if $pos.pos < $pos.text.chars {
            &continue($match.clone(
                pos => $pos.clone(pos => $pos.pos + 1),
            ));
        }
        else {
            $match.backtrack()();
        }
    });
}

# vim: ft=perl6 :
