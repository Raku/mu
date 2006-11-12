use v6-alpha;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {
        'package ' ~ $.name ~ ";\n" ~ (@.body.>>emit).join( ";\n" )
    }
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
        '(' ~ (@.seq.>>emit).join(', ') ~ ')';
    }
}

class Lit::Array {
    has @.array;
    method emit {
        '[' ~ (@.array.>>emit).join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash;
    method emit {
        my $fields := @.hash;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
        ## '{' ~ ((%.hash.kv).map(sub ($k, $v) { $k.perl ~ ' => ' ~ $v.emit})).join(', ') ~ '}';
        ## '{' ~ %.hash.kv.map(sub ($k, $v) { $k.perl ~ ' => ' ~ $v.emit}).join(', ') ~ '}';
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit {
        # $.class ~ '->new( ' ~ @.fields.>>emit.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit ~ ' => ' ~ $_[1].emit}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        $.class ~ '->new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit {
        $.obj.emit ~ '->[' ~ $.index.emit ~ ']';
        # TODO
        # if ($.obj.isa(Lit::Seq)) {
        #    $.obj.emit ~ '[' ~ $.index.emit ~ ']';
        # }
        # else {
        #    $.obj.emit ~ '->[' ~ $.index.emit ~ ']';
        # }
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

class Proto {
    has $.name;
    method emit {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    has $.hyper;
    method emit {
        my $call := '->' ~ $.method ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
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
        $.code ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        # '(' ~ $.code.emit ~ ')->(' ~ @.arguments.>>emit.join(', ') ~ ')';
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
        'do { if (' ~ $.cond.emit ~ ') { ' ~ (@.body.>>emit).join(';') ~ ' } else { ' ~ (@.otherwise.>>emit).join(';') ~ ' } }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit {
        'do { for my ' ~ $.topic.emit ~ ' (' ~ $.cond.emit ~ ') { ' ~ (@.body.>>emit).join(';') ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.var;
    method emit {
        $.decl ~ ' ' ~ $.var.emit
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit {
        " # Signature - TODO \n"
    };
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant; 
        # say $invocant.emit;
        my $pos := $sig.positional;
        my $str := '';
        my $i := 1;
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i := $i + 1;
        };
        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        ## my $invocant := $sig.invocant; 
        # say $invocant.emit;
        my $pos := $sig.positional;
        my $str := '';
        my $i := 0;
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i := $i + 1;
        };
        'sub ' ~ $.name ~ ' { ' ~ 
          ## 'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Do {
    has @.block;
    method emit {
        'do { ' ~ 
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Use {
    has $.mod;
    method emit {
        'use ' ~ $.mod
    }
}
