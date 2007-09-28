use v6-alpha;

class KindaPerl6::Visitor::EmitTokenC {

    method visit ( $node, $node_name ) {
        $node.emit_c;
    }

}
class Token {
    method emit_c {
        'match* ' ~ $.name ~ ' (char *str,int pos) {match* m = malloc(sizeof(match));m->match_str = str;m->from=pos;m->boolean = (' ~ ($.regex).emit_c ~ ');m->to = pos;return m;}' ~ Main::newline();
    }
}
class CompUnit {
    method emit_c {
        $.body.emit_c;
    }
}
class Lit::Code {
    method emit_c {
        my $source := '';
        for @.body -> $node {
            if ($node.isa('Token')) {
                $source := $source ~ $node.emit_c;
            }
        }
        $source;
    };
}
class Rule::Or {
    method emit_c {
        '({int saved_pos=pos;' ~ (@.or.>>emit_c).join('||') ~ '|| (pos=saved_pos,0);})';
    }
}
class Rule::Concat {
    method emit_c {
        '(' ~ (@.concat.>>emit_c).join('&&') ~ ')';
    }
}
class Rule::Constant {
    method emit_c {
        '(strncmp("' ~ $.constant ~ '",str+pos,' ~ length($.constant) ~ ') == 0 && (pos += ' ~ length($.constant) ~ '))';
    }
}
class Rule::Subrule {
    method emit_c {
        '({match* submatch='~$.metasyntax~'(str,pos);pos = submatch->to;int boolean = submatch->boolean;free(submatch);boolean;})';
    }
}
