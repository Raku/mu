role Parse::Rule::Media::Text;

use Parse::Rule::Medium;
does Parse::Rule::Medium;

=pod

C<Text> is the most common medium.  It stores a finite string and a position
within that string, and it provides the fundamental operations on strings as
combinators.

    $.text     - The string
    $.pos      - The offset from 0 in that string

=cut

role Parse::Rule::Media::Text::Pos { 
    does Parse::Rule::Medium::Pos;

    has Str $.text;
    has Int $.pos;
}

submethod BUILD () {
    $.Pos = Parse::Rule::Media::Text::Pos;
}

method literal(Str $text) {...}

method any_char() {...}

method beginning_of_string () {
    ./assertion: -> $m {
        my $pos = $m.pos;
        $pos.pos == 0;
    }
}

method end_of_string () {
    ./assertion: -> $m {
        my $pos = $m.pos;
        $pos.pos == $pos.text.chars;
    }
}

method beginning_of_line () {
    ./assertion: -> $m {
        my $pos = $m.pos;
        $pos.pos == 0 || $pos.text.substr($pos.pos-1, 1) eq "\n";
    }
}

method end_of_line () {
    ./assertion: -> $m {
        my $pos = $m.pos;
        $pos.pos == $pos.text.chars 
            || $pos.text.substr($pos.pos, 1) eq "\n";
    }
}

# vim: ft=perl6 :
