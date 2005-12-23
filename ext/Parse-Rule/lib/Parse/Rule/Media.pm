module Parse::Rule::Media;

use Parse::Rule::Core;

class Text {
    is Medium;
    has Str $.text;
    has Int $.pos;

    sub literal (Str $text) {
        Parser.new: parse => sub ($match, &continue) {
            my $pos = $match.pos;
            my $epos = $pos.pos + $text.chars;
            if $pos.text.substr($pos.pos, $text.chars) eq $text {
                &continue(Result.new(
                    pos => $pos.clone(
                        pos => $epos,
                    ),
                    backtrack => $match.backtrack,
                    value => Match.new(
                        start => $pos.pos,
                        end => $epos,
                    ),
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
                &continue(Result.new(
                    pos => $pos.clone(
                        pos => $pos.pos + 1,
                    ),
                    backtack => $match.backtrack,
                    value => Match.new(
                        start => $pos.pos,
                        end => $pos.pos + 1,
                    ),
                ));
            }
            else {
                $match.backtrack()();
            }
        };
    }
}

# vim: ft=perl6 :
