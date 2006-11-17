use v6-alpha;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {
        'package ' ~ $.name ~ "; " ~ 
        'sub new { shift; bless { @_ }, "' ~ $.name ~ '" }' ~ " " ~
        (@.body.>>emit).join( "; " )
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
    method emit { '\'' ~ $.buf ~ '\'' }
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
           ( $.twigil eq '.' )
        ?? ( '$_[0]->{' ~ $.name ~ '}' )
        !!  (    ( $.name eq '/' )
            ??   ( $table{$.sigil} ~ 'MATCH' )
            !!   ( $table{$.sigil} ~ $.name )
            )
    };
    method name {
        $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] := [1, [2, 3]]
            
            my $a := $.parameters.array;
            my $b := $.arguments.array;
            my $str := 'do { ';
            my $i := 0;
            for @$a -> $var { 
                my $bind := ::Bind( 'parameters' => $var, 'arguments' => ($b[$i]) );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        }
        else {
            $.parameters.emit ~ ' = ' ~ $.arguments.emit;
        }
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
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
                return 
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')' ~ ' } @{ ' ~ $.invocant.emit ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $.invocant.emit ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')';
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';  
        };
        
        my $call := '->' ~ $meth ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        if ($.hyper) {
            '[ map { $_' ~ $call ~ ' } @{ ' ~ $.invocant.emit ~ ' } ]';
        }
        else {
            $.invocant.emit ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit {
        
        my $code := $.code;

        if $code eq 'say'        { return 'Main::say('   ~ (@.arguments.>>emit).join(', ') ~ ')' };
        if $code eq 'print'      { return 'Main::print(' ~ (@.arguments.>>emit).join(', ') ~ ')' };

        if $code eq 'array'      { return '@{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ? 0 : 1)' };
        if $code eq 'prefix:<?>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ? 1 : 0)' };

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return '('  ~ (@.arguments.>>emit).join(' . ')  ~ ')' };
        if $code eq 'infix:<+>'  { return '('  ~ (@.arguments.>>emit).join(' + ')  ~ ')' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit).join(' - ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit).join(' && ') ~ ')' };
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit).join(' || ') ~ ')' };
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit).join(' eq ') ~ ')' };
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit).join(' ne ') ~ ')' };
 
        if $code eq 'ternary:<?? ::>' { 
            return '(' ~ (@.arguments[0]).emit ~
                 ' ? ' ~ (@.arguments[1]).emit ~
                 ' : ' ~ (@.arguments[2]).emit ~
                  ')' };
        
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
        'do { for my ' ~ $.topic.emit ~ ' ( ' ~ $.cond.emit ~ ' ) { ' ~ (@.body.>>emit).join(';') ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.var;
    method emit {
        my $decl := $.decl;
        my $name := $.var.name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}' )
        !! $.decl ~ ' ' ~ $.var.emit;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
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
