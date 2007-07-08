
use v6-alpha;

=begin

This visitor preprocess the Regex capture counts.

* capture_count( 0, 0, {} )

Positional parameters:
    - $count: the current positional-capture number
        /(.)(.)/
              ^--- count := 2
            ^----- count := 1
         ^-------- count := 0
    - $quantified: whether this node is inside a quantifier
    - %seen: which named-captures were already seen

=end

class KindaPerl6::Visitor::RegexCapture {
    method visit ( $node, $node_name ) {
        if ( $node_name eq 'Token' )
        {
            #say "RegexCapture: Token";
            ($node.regex).capture_count( 0, 0, {} );
            return $node;
        };
        return;
    };
}


# node "aspects"

class Rule::Quantifier {
    method capture_count( $count, $quantified, $seen ) {
        $.term.capture_count( $count, 1, $seen );
        $count;
    }
}

class Rule::Or {
    method capture_count( $count, $quantified, $seen ) {
        my $max := $count;
        for @.or -> $regex {
            #say "Or";
            my $last := $regex.capture_count( $count, $quantified, $seen );
            if $last > $max {
                $max := $last;
            }
        }
        # return the maximum count
        $max;
    }
}

class Rule::Concat {
    method capture_count( $count, $quantified, $seen ) {
        for @.concat -> $regex {
            #say "Concat";
            $count := $regex.capture_count( $count, $quantified, $seen );
        }
        $count;
    }
}

class Rule::Subrule {
    method capture_count( $count, $quantified, $seen ) {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax ~ ' ... TODO '
            !! ( '\'$\'.$GLOBAL::_Class.\'::_regex_' ~ $.metasyntax ~ '\'' );
        # TODO - if seen, go back to previous and mark as capture-to-array
        $seen{ $meth } := $seen{ $meth } + 1;
        $count;
    }
}

class Rule::SubruleNoCapture {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::Var {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::Constant {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::Dot {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::SpecialChar {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::Block {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::InterpolateVar {
    method capture_count( $count, $quantified, $seen ) {
        $count;
    }
}

class Rule::NamedCapture {
    method capture_count( $count, $quantified, $seen ) {
        say '# TODO: named capture ' ~ $.ident ~ ' := ' ~ $.rule.capture_count ~ '';
        die();
    }
}

class Rule::Before {
    method capture_count( $count, $quantified, $seen ) {
        say "TODO Before";
        die();
    }
}

class Rule::NotBefore {
    method capture_count( $count, $quantified, $seen ) {
        say "TODO NotBefore";
        die();
    }
}

class Rule::NegateCharClass {
    # unused
    method capture_count( $count, $quantified, $seen ) {
        say "TODO NegateCharClass";
        die();
    }
}

class Rule::CharClass {
    # unused
    method capture_count( $count, $quantified, $seen ) {
        say "TODO CharClass";
        die();
    }
}

class Rule::Capture {
    method capture_count( $count, $quantified, $seen ) {
        $.position := $count;
        $count + 1;
    }
}
