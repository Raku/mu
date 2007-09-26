use v6-alpha;

class KindaPerl6::Visitor::EmitTokenC {

    method visit ( $node, $node_name ) {
        $node.emit_c;
    }

}
class Token {
    method emit_c {
        return 'match ' ~ $.name ~ ' (char *str,int str_len,int pos) {match m;m.match_str = str;m.from=pos;m.boolean = (' ~ ($.regex).emit_c ~ ');m.to = pos;return m;}';
    }
}
class CompUnit {
    method emit_c {
        $.body.emit_c;
    }
}
class Lit::Code {
    method emit_c {
        (@.body.>>emit_c).join('');
    };
}
class Rule::Or {
    method emit_c {
        return '({int saved_pos=pos;' ~ (@.or.>>emit_c).join('||') ~ '|| (pos=saved_pos,0);})';
    }
}
class Rule::Concat {
    method emit_c {
        return '(' ~ (@.concat.>>emit_c).join('&&') ~ ')';
    }
}
class Rule::Constant {
    method emit_c {
        return '(str_len >= pos + ' ~ length($.constant) ~ '&& strncmp("' ~ $.constant ~ '",str+pos,' ~ length($.constant) ~ ') == 0 && (pos += ' ~ length($.constant) ~ '))';
    }
    method emit_perl5 {
          '{ ' 
        ~ self.emit_declarations ~ self.emit_body
        ~ ' }';
    };
}
