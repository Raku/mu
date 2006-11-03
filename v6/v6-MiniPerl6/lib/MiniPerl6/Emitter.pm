use v6-alpha;

class CompUnit {
    has $.class;
    has %.attributes;
    has %.methods;
    has $.body;
}

class Val::Int {
    has $.int;
    method emit { $.int }
}

class Val::Bit {
    has $.bit;
    method emit { $.bit }
}

class Val::Num {
    has $.num;
    method emit { $.num }
}

class Val::Buf {
    has $.buf;
    method emit { $.buf.perl }
}

class Val::Undef {
    method emit { '(undef)' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit {
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method emit {
        '(' ~ @.seq.>>emit.join(', ') ~ ')';
    }
}

class Lit::Array {
    has @.array;
    method emit {
        '[' ~ @.seq.>>emit.join(', ') ~ ']';
    }
}

class Lit::Hash {
    has %.hash;
    method emit {
        '{' ~ %.hash.kv.map(sub ($k, $v) { $k.perl ~ ' => ' ~ $v.emit}).join(', ') ~ '}';
    }
}

class Lit::Code {
    # XXX
}

class Index {
    has $.obj;
    has $.index;
    method emit {
        if ($.obj.isa(Lit::Seq)) {
            $.obj.emit ~ '[' ~ $.index.emit ~ ']';
        }
        else {
            $.obj.emit ~ '->[' ~ $.index.emit ~ ']';
        }
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit {
        $.obj.emit ~ '->{' ~ $.index.emit ~ '}';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
        $table{$.sigil} ~ $.name
    }
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        $.parameters.emit ~ ' = ' ~ $.arguments.emit
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit {
        my $call := '->' ~ $.method ~ '(' ~ @.arguments.>>emit.join(', ') ~ ')';
        if ($.hyper) {
            '(map { $_' ~ $call ~ ' } @{ ' ~ $.invocant.emit ~ ' } )';
        }
        else {
            $.invocant.emit ~ $call;
        }
    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit {
        '(' ~ $.code.emit ~ ')->(' ~ @.arguments.>>emit.join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit {
        'return(' ~ $.result.emit ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit {
        'do { if (' ~ $.cond.emit ~ ') { ' ~ @.body.>>emit ~ ' } else { ' ~ @.otherwise.>>emit ~ ' } }';
    }
}
