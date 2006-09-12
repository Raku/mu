use v6-alpha;

class Automata::Cellular-0.1 
    does Automata::Cellular::Rule
{
    has Automata::Cellular::Rule $.rule;
    has Bool @.state is rw;
    has Int  $.steps;
    has Int  $.display_width;
    has Int  $.stage is rw;

    submethod BUILD (:$.rule,:@state,:$.steps,:$.display_width) {
        $.stage = 1;
    }

    method prettystate (Str $true, Str $false) {
        my $state = (+<<@.state[$.steps..(@.state.elems() - $.steps)]).join(""); 
        $state ~~ s:g/0/$false/;
        $state ~~ s:g/1/$true/;
        "Stage $.stage: $state";
    }

    method postfix:<++> {
        my @old_state = @.state;
        for ( 0 .. (@old_state.elems - 2) ) -> $index {
            my $index_key =
                :2((+<<@old_state[ $index .. $index + 2 ]).join(""));
            @.state[ $index + 1 ] = $.rule.rule{$index_key};
        }

        $.stage = $.stage + 1; # since we are overloading ++ ;)
        if $.stage >= $.steps { return Bool::False; }
        else { return Bool::True; }
    }





} # class Automata::Cellular
