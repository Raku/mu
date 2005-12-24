module Parse::Rule::Media;

use Parse::Rule::Core;
use Parse::Rule::Combinators;

=pod

C<Text> is the most common medium.  It stores a finite string and a position
within that string, and it provides the fundamental operations on strings as
combinators.

    $.text     - The string
    $.pos      - The offset from 0 in that string
    literal    - A combinator that matches a literal string within a match of
                 Text.
    any_char   - A combinator that matches any single character.

=cut

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

    sub beginning_of_string () {
        assertion -> $m {
            my $pos = $m.pos;
            $pos.pos == 0;
        }
    }

    sub end_of_string () {
        assertion -> $m {
            my $pos = $m.pos;
            $pos.pos == $pos.text.chars;
        }
    }

    sub beginning_of_line () {
        assertion -> $m {
            my $pos = $m.pos;
            $pos.pos == 0 || $pos.text.substr($pos.pos-1, 1) eq "\n";
        }
    }

    sub end_of_line () {
        assertion -> $m {
            my $pos = $m.pos;
            $pos.pos == $pos.text.chars 
                || $pos.text.substr($pos.pos, 1) eq "\n";
        }
    }
}

# vim: ft=perl6 :
