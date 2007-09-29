use v6-alpha;

class KindaPerl6::Visitor::EmitTokenC {

    method visit ( $node, $node_name ) {
        $node.emit_c;
    }

}
class Token {
    method emit_c {
        'match* ' ~ Main::mangle_ident($KindaPerl6::Visitor::EmitPerl5::current_compunit~'::'~$.name) ~ ' (char *str,int pos) {match* m = new_match(str,pos);m->boolean = (' ~ ($.regex).emit_c ~ ');m->to = pos;return m;}' ~ Main::newline();
    }
}
class CompUnit {
    method emit_c {
        $KindaPerl6::Visitor::EmitPerl5::current_compunit := $.name;
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
class Rule::Block {
    method emit_c {
        'printf("Rule::Block stub:'~$.closure~'\n")';
    }
}
class Rule::Subrule {
    method emit_c {
        '({match* submatch='~Main::mangle_ident($KindaPerl6::Visitor::EmitPerl5::current_compunit~'::'~$.metasyntax)~'(str,pos);pos = submatch->to;int boolean = submatch->boolean;free(submatch);boolean;})';
    }
}
class Rule::SubruleNoCapture {
    method emit_c {
        '({match* submatch='~Main::mangle_ident($KindaPerl6::Visitor::EmitPerl5::current_compunit~'::'~$.metasyntax)~'(str,pos);pos = submatch->to;int boolean = submatch->boolean;free(submatch);boolean;})';
    }
}
class Rule::Dot {
    method emit_c {
        'printf("Rule::Dot stub\n")'
    }
}
class Rule::SpecialChar {
    method emit_c {
        'printf("Rule::SpecialChar stub\n")';
    }
}
class Rule::Before {
    method emit_c {
        'printf("Rule::Before stub")';
    }
}
