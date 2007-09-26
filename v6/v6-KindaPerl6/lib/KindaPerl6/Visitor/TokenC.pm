use v6-alpha;

class KindaPerl6::Visitor::TokenC {

    method visit ( $node, $node_name ) {
        $node.emit_c;
    }

}
class Token {
    method emit_c {
        return 'match ' ~ $.name ~ ' (char *str,int str_len,int pos) {(' ~ ($.regex).emit_c ~ ') || printf("token failed at %d\n",pos);printf("match ended at %d\n",pos);}';
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
        return '(str_len >= pos + ' ~ length($.constant) ~ '&& strncmp("' ~ $.constant ~ '",str+pos,' ~ length($.constant) ~ ') == 0 && (pos += ' ~ length($.constant) ~ '))' ;
    }
    method emit_perl5 {
          '{ ' 
        ~ self.emit_declarations ~ self.emit_body
        ~ ' }';
    };
}
