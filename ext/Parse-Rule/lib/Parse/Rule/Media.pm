module Parse::Rule::Media;

use Parse::Rule::Core;

class Text {
    is Medium;
    has Str $.text;
    has Int $.pos;

    sub literal (Str $text) {
        Parser.new: parse => sub ($match, &continue) {
            my $pos = $match.pos;
            if $pos.text.substr($pos.pos, $text.chars) eq $text {
                &continue($match.clone(
                    pos => $pos.clone(pos => $pos.pos + $text.chars),
                ));
            }
            else {
                $match.backtrack()();
            }
        }
    }

    sub any_char () {
        Parser.new: parse => sub ($match, &continue) {
            my $pos = $match.pos;
            if $pos.pos < $pos.text.chars {
                &continue($match.clone(
                    pos => $pos.clone(pos => $pos.pos + 1),
                ));
            }
            else {
                $match.backtrack()();
            }
        }
    }
}

# vim: ft=perl6 :
