
use v6-alpha;

class KindaPerl6::Visitor::Emit::Ruby {
    has $.visitor_args;
    # This visitor is a perl5 emitter

    method visit ( $node ) {
        $node.emit_ruby($.visitor_args{'secure'});
    };

}

class CompUnit {
    sub set_secure_mode( $args_secure ) {
        my $value := '0';
        if ($args_secure) { $value := '1' };
        return 'use constant KP6_DISABLE_INSECURE_CODE => ' ~ $value ~ ';' ~ Main::newline();
    };
    method emit_ruby( $args_secure ) {
        $KindaPerl6::Visitor::Emit::Ruby::current_compunit := $.name;
        my $source := '';
        if ($.body) {
            $source := $.body.emit_ruby;
        };
        '# Machine-generated ruby code.' ~ Main::newline()
        ~ '# Ruby version >= 1.9.0 2007-12-25 is needed.' ~ Main::newline()
        ~ 'require \'kp6_runtime\'' ~ Main::newline()
        ~ Main::newline()
        ~ $source ~ Main::newline();
    }
}

class Val::Int {
    method emit_ruby {
        # $.int
        ' ' ~ $.int ~ '';
    }
}

class Val::Bit {
    method emit_ruby {
        # $.bit
        ' Bit.new(' ~ ($.bit ?? 'true' !! 'false') ~ ')'
    }
}

class Val::Num {
    method emit_ruby {
        #$.num
        ' ' ~ $.num ~ '';
    }
}

class Val::Buf {
    method emit_ruby {
        # '\'' ~ $.buf ~ '\''
        ' ' ~ Main::singlequote() ~ Main::mangle_string( $.buf ) ~ Main::singlequote;
    }
}

class Val::Char {
    method emit_ruby {
        ' ' ~ $.char ~ '.chr()';
    }
}

class Val::Undef {
    method emit_ruby {
        ' Undef.new()'
    }
}

class Val::Object {
    method emit_ruby {
        die 'Emitting of Val::Object not implemented';
        # 'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Native::Buf {
    method emit_ruby {
        die 'Emitting of Native::Buf not implemented';
        # '\'' ~ $.buf ~ '\''
    }
}

class Lit::Seq {
    method emit_ruby {
        '(' ~ (@.seq.>>emit_ruby).join(', ') ~ ')';
    }
}

class Lit::Array {
    method emit_ruby {
        # # this is not a Perl 6 object,
        # # objects are created with a high-level Array.new or List.new
        '[' ~ (@.array.>>emit_ruby).join(', ') ~ ']';
    }
}

class Lit::Hash {
    method emit_ruby {
        # # this is not a Perl 6 object,
        # # objects are created with a high-level Hash.new
        my $fields := @.hash;
        my $str := '';
        my $field;
        for @$fields -> $field {
            $str := $str ~ ', ' ~ ($field[0]).emit_ruby
                         ~ ': ' ~ ($field[1]).emit_ruby ~ '';
        };
        $str := substr( $str , 1 )
        '{' ~ $str ~ ' }' ~ Main::newline();
    }
}

class Lit::Pair {
    method emit_ruby {
        ' Pair.new(' ~ $.key.emit_ruby ~ ', ' ~ $.value.emit_ruby ~ ')';
    }
}

class Lit::NamedArgument {
    method emit_ruby {
        ' Ruddy::NamedArgument.new(' ~ $.key.emit_ruby ~ ', '
        ~ ( defined($.value) ?? $.value.emit_ruby !! 'Undef.new' )
        ~ ')'
    }
}

class Lit::SigArgument {
    method emit_ruby {

        ' Signature::Item.new('
        ~ '{ '

        ~     'sigil:  \'' ~ $.key.sigil  ~ '\', '
        ~     'twigil: \'' ~ $.key.twigil ~ '\', '
        ~     'name:   \'' ~ $.key.name   ~ '\', '

        ~     'value:   ' ~ ( defined($.value) ?? $.value.emit_ruby !! 'undef' ) ~ ', '  # XXX

        ~     'has_default:     ' ~ $.has_default.emit_ruby  ~ ', '
        ~     'is_named_only:   ' ~ $.is_named_only.emit_ruby  ~ ', '
        ~     'is_optional:     ' ~ $.is_optional.emit_ruby    ~ ', '
        ~     'is_slurpy:       ' ~ $.is_slurpy.emit_ruby      ~ ', '
        ~     'is_multidimensional:  ' ~ $.is_multidimensional.emit_ruby  ~ ', '
        ~     'is_rw:   ' ~ $.is_rw.emit_ruby          ~ ', '
        ~     'is_copy: ' ~ $.is_copy.emit_ruby        ~ ', '

        ~ ' } )' ~ Main::newline();
    }
}

class Lit::Code {
    method emit_ruby {
        if ($.CATCH) {
          'do { eval {'
        ~ self.emit_declarations ~ self.emit_body
        ~ '};if ($@) {' ~ $.CATCH.emit_ruby ~ '}}';
        }
        else {
            my $vars;
            for @($.pad.lexicals) -> $name {
                $vars := $vars ~ ',Variable.new'
            }
            $vars := substr( $vars, 1 );
            '(->('
            ~ ((@($.pad.lexicals)).>>emit_ruby).join(', ')
            ~ '){ ' ~ Main::newline()
            ~ self.emit_body
            ~ '}).(' ~ $vars ~ ')' ~ Main::newline();
        }
    };
    method emit_body {
        (@.body.>>emit_ruby).join('; ');
    };
    method emit_signature {
        $.sig.emit_ruby
    };
    method emit_declarations {
        my $s;
        my $name;
        for @($.pad.lexicals) -> $name {
            my $decl := Decl.new(
                decl => 'my',
                type => '',
                var  => Var.new(
                    sigil     => '',
                    twigil    => '',
                    name      => $name,
                    namespace => [ ],
                ),
            );
            $s := $s ~ $name.emit_ruby ~ ';' ~ Main::newline();
        };
        return $s;
    };
    method emit_arguments {
        my $array_  := Var.new( sigil => '@', twigil => '', name => '_',       namespace => [ ], );
        my $hash_   := Var.new( sigil => '%', twigil => '', name => '_',       namespace => [ ], );
        my $CAPTURE := Var.new( sigil => '$', twigil => '', name => 'CAPTURE', namespace => [ ],);
        my $CAPTURE_decl := Decl.new(decl=>'my',type=>'',var=>$CAPTURE);
        my $str := '';
        $str := $str ~ $CAPTURE_decl.emit_ruby;
        $str := $str ~ (Decl.new(decl=>'my',type=>'',var=>$array_)).emit_ruby;
        $str := $str ~ '::DISPATCH_VAR($CAPTURE,"STORE",::CAPTURIZE(\@_));';

        # XXX s/assign/bind/ ?
        my $bind_array :=
                    Assign.new(parameters=>$array_,arguments=> Call.new(invocant => $CAPTURE,method => 'array',arguments => []));
        $str := $str ~ $bind_array.emit_ruby ~ ';';

        my $bind_hash :=
                    Bind.new(parameters=>$hash_, arguments=> Call.new(invocant => $CAPTURE,method => 'hash', arguments => []));
        $str := $str ~ $bind_hash.emit_ruby ~ ';';

        my $i := 0;
        my $field;
        $str := $str ~ '{ my $_param_index = 0; ';
        for @($.sig.positional) -> $field {

            my $bind_named := Bind.new(
                    parameters => $field.key,
                    arguments  => Call.new(
                            invocant  => $hash_,
                            arguments => [ Val::Buf.new( buf => ($field.key).name ) ],
                            method    => 'LOOKUP',
                        ),
                );
            my $bind_default := Bind.new(
                    parameters => $field.key,
                    arguments  => $field.value,
                );

            $str := $str
                    ~ ' if ( ::DISPATCH( $GLOBAL::Code_exists, '
                    ~   ' \'APPLY\', '
                    ~   ' ::DISPATCH( '
                    ~       ' $Hash__, \'LOOKUP\', '
                    ~       ' ::DISPATCH( $::Str, \'new\', \'' ~ ($field.key).name ~ '\' ) '
                    ~   ' ) )->{_value} '
                    ~ ' ) '
                    ~ ' { '
                    ~     $bind_named.emit_ruby
                    ~ ' } '
                    ~ ' elsif ( ::DISPATCH( $GLOBAL::Code_exists, '
                    ~   ' \'APPLY\', '
                    ~   ' ::DISPATCH( '
                    ~       ' $List__, \'INDEX\', '
                    ~       ' ::DISPATCH( $::Int, \'new\', $_param_index ) '
                    ~   ' ) )->{_value} '
                    ~ ' ) '
                    ~ ' { '
                    ~     ($field.key).emit_ruby
                    ~         ' = ::DISPATCH( '
                    ~       ' $List__, \'INDEX\', '
                    ~       ' ::DISPATCH( $::Int, \'new\', $_param_index++ ) '
                    ~   ' ); '
                    ~ ' } ';
            if ($field.has_default).bit {
                $str := $str
                    ~ ' else { '
                    ~     $bind_default.emit_ruby
                    ~ ' } ';
            }
            $i := $i + 1;
        };
        $str := $str ~ '} ';

        return $str;
    };
}

class Lit::Object {
    method emit_ruby {
        # $.class ~ '->new( ' ~ @.fields.>>emit_ruby.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit_ruby ~ ' => ' ~ $_[1].emit_ruby}).join(', ') ~ ')';
        my $field;
        for @$fields -> $field {
            $str := $str
                ~ ', Ruddy::NamedArgument.new('
                ~ ($field[0]).emit_ruby ~ ', '
                ~ ($field[1]).emit_ruby
                ~ ')'
                ;
        };
	$str := substr( $str , 1 );
        ' ' ~ $.class ~ '.m_new(nil,nil,[ ' ~ $str ~ ' ])' ~ Main::newline();
    }
}


class Assign {
    method emit_ruby {
        # TODO - same as ::Bind

        my $node := $.parameters;

        if $node.isa( 'Var' ) && @($node.namespace)
        {
            # it's a global,
            # and it should be autovivified

            $node :=
                Apply.new(
                    code => Var.new(
                        name      => 'ternary:<?? !!>',
                        twigil    => '',
                        sigil     => '&',
                        namespace => [ 'GLOBAL' ],
                    ),
                    arguments => [
                       Apply.new(
                            arguments => [ $node ],
                            code => Var.new( name => 'VAR_defined', twigil => '', sigil => '&', namespace => [ 'GLOBAL' ] ),
                        ),
                        $node,
                        Bind.new(
                            'parameters' => $node,
                            'arguments'  => Call.new(
                                'invocant' => Var.new( name => '::Scalar', twigil => '', sigil => '$', namespace => [ ] ),
                                'method'   => 'new',
                                'hyper'    => '',
                            ),
                        )
                    ],
                );

        };

        ' ' ~ $node.emit_ruby ~ '._(' ~ $.arguments.emit_ruby ~ ')';
    }
}

class Var {
    method emit_ruby {
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

        if $.twigil eq '.' {
            return '::DISPATCH( $self, "' ~ $.name ~ '" )'  ~ Main::newline()
        };
        if $.twigil eq '!' {
            return '$self->{_value}{"' ~ $.name ~ '"}'  ~ Main::newline()
        };

        if $.name eq '/' {
            return $table{$.sigil} ~ 'MATCH'
        };

        if @($.namespace) {
            my $s;
            my $var := Main::mangle_name( $.sigil, $.twigil, $.name, $.namespace );

            if $.sigil eq '$' {
                $s := '$::Scalar';
            };
            if $.sigil eq '&' {
                $s := '$::Routine';
            };
            if $.sigil eq '%' {
                $s := '$::HashContainer';
            };
            if $.sigil eq '@' {
                $s := '$::ArrayContainer';
            };

            #return $var;
            # XXX doesn't work???
            return ' ( '
                ~ $var ~ ' = ' ~ $var ~ ' || ::DISPATCH( ' ~ $s ~ ', "new", ) '
                ~ ' ) ' ~ Main::newline();
        }

        return Main::mangle_name_ruby( $.sigil, $.twigil, $.name, $.namespace );
    };
    #method perl {
    #    # this is used by the signature emitter
    #    # XXX rename this node, it may clash with a User class
    #      '::DISPATCH( $::Var, "new", { '
    #    ~     'sigil  => \'' ~ $.sigil  ~ '\', '
    #    ~     'twigil => \'' ~ $.twigil ~ '\', '
    #    ~     'name   => \'' ~ $.name   ~ '\', '
    #    ~     'namespace => [ ], '
    #    ~ '} )' ~ Main::newline()
    #}
}

class Bind {
    method emit_ruby {

        # XXX - replace Bind with .BIND
        if      $.parameters.isa('Call')
            ||  (   $.parameters.isa('Var')
                &&  ( ($.parameters).sigil eq '@' )
                )
        {
            return
                  '::DISPATCH_VAR( '
                ~   $.parameters.emit_ruby
                ~   ', "BIND", '
                ~   $.arguments.emit_ruby
                ~ ' )'
        };

        # XXX - replace Bind with Assign
        #if $.parameters.isa('Call')
        #{
        #    return Assign.new(parameters=>$.parameters,arguments=>$.arguments).emit_ruby;
        #};

        #my $str := '::MODIFIED(' ~ $.parameters.emit_ruby ~ ');' ~ Main::newline();
        #$str := $str ~ $.parameters.emit_ruby ~ ' = ' ~ $.arguments.emit_ruby;
        #return 'do {'~$str~'}';
        $.parameters.emit_ruby
        ~ ' = ' ~ $.arguments.emit_ruby
        ~ Main::newline();
    }
}

class Proto {
    method emit_ruby {
        return ' '~$.name;
    }
}

class Call {
    method emit_ruby {
        my $invocant;
        if $.invocant.isa( 'Proto' ) {

            if $.invocant.name eq 'self' {
                $invocant := '$self';
            }
            else {
                $invocant := $.invocant.emit_ruby;
            }

        }
        else {
            $invocant := $.invocant.emit_ruby;
        };
        if $invocant eq 'self' {
            $invocant := '$self';
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';
        };

        my $call := (@.arguments.>>emit_ruby).join(', ');
        if ($.hyper) {
            # TODO - hyper + role
              '::DISPATCH( $::List, "new", { _array => [ '
            ~     'map { ::DISPATCH( $_, "' ~ $meth ~ '", ' ~ $call ~ ') } '
            ~          '@{ ::DISPATCH( ' ~ $invocant ~ ', "array" )->{_value}{_array} } '
            ~ '] } )' ~ Main::newline();
        }
        else {
            if ( $meth eq '' ) {
                # $var.()
                '::DISPATCH( ' ~ $invocant ~ ', \'APPLY\', ' ~ $call ~ ' )' ~ Main::newline()
            }
            else {
                  '::DISPATCH( '
                ~ $invocant ~ ', '
                ~ '\'' ~ $meth ~ '\', '
                ~ $call
                ~ ' )'
                ~ Main::newline()
            };
        };


    }
}

class Apply {
    method emit_ruby {


        if  ( $.code.isa('Var') ) && ( $.code.name eq 'self' )
        {
            # dlocaus @ #perl6 irc.freenode.net
            # fglock's comment on this work around
            # http://irclog.perlgeek.de/perl6/2007-11-21#i_148959
            # He stated that the code is return $self, instead of trying to parse
            # self().
            # Removing this hack breaks the test cases when you do:
            # perl Makefile.PL ; make forcerecompile ; make test
            # November 21st, 2007 10:51am PDT.
            return '$self';
        }

        if  ( $.code.isa('Var') ) && ( $.code.name eq 'infix:<&&>' )
        {
            # hack for shortcircuiting "&&"
            # as an alternative hack, see Visitor::ShortCircuit
            return  'do { '
                        ~ 'my $_tmp1 = ' ~ ((@.arguments[0]).emit_ruby) ~ '; '
                        ~ '::DISPATCH( $_tmp1, "true" )->{_value} '
                        ~ '? ' ~ ((@.arguments[1]).emit_ruby)
                        ~ ': ::DISPATCH( $::Bit, "new", 0 )'
                ~ ' }' ~ Main::newline();
        }

        if  ( $.code.isa('Var') ) && ( $.code.name eq 'infix:<||>' )
        {
            # hack for shortcircuiting "||"
            # as an alternative hack, see Visitor::ShortCircuit
            return  'do { '
                        ~ 'my $_tmp1 = ' ~ ((@.arguments[0]).emit_ruby) ~ '; '
                        ~ '::DISPATCH( $_tmp1, "true" )->{_value} '
                        ~ '? $_tmp1'
                        ~ ': ' ~ ((@.arguments[1]).emit_ruby)
                ~ ' }' ~ Main::newline();
        }

        if  ( $.code.isa('Var') ) && ( $.code.name eq 'make' )
        {
            # hack for "make" (S05)
            return  '::DISPATCH_VAR( '
                        ~ '$GLOBAL::_REGEX_RETURN_, "STORE", ' ~ ((@.arguments[0]).emit_ruby) ~ ''
                ~ ' )' ~ Main::newline();
        }

        #return  '::DISPATCH( ' ~ $.code.emit_ruby ~ ', \'APPLY\', ' ~ (@.arguments.>>emit_ruby).join(', ') ~ ' )' ~ Main::newline();
        return  ' ' ~ $.code.emit_ruby ~ '.(cx(' ~ (@.arguments.>>emit_ruby).join(', ') ~ '))' ~ Main::newline();
    }
}

class Return {
    method emit_ruby {
        # call .FETCH just in case it's a Container
        # 'return( ::DISPATCH(' ~ $.result.emit_ruby ~ ', "FETCH" ) )' ~ Main::newline();

        #'do { print Main::perl(caller(),' ~ $.result.emit_ruby ~ '); return(' ~ $.result.emit_ruby ~ ') }';
        'return(' ~ $.result.emit_ruby ~ ')' ~ Main::newline();
    }
}

class If {
    method emit_ruby {
        'if (' ~ $.cond.emit_ruby ~ ').is_true6? ' ~ Main::newline()
        ~ ( $.body
            ?? ' ' ~ $.body.emit_ruby ~ ''
            !! ''
          )
        ~ ( $.otherwise
            ?? ' else ' ~ Main::newline() ~ $.otherwise.emit_ruby ~ ' '
            !! ' else; Bit.new(false); '
          )
        ~ Main::newline() ~ 'end' ~ Main::newline();
    }
}

class While {
    method emit_ruby {
        my $cond := $.cond;
        if   $cond.isa( 'Var' )
          && $cond.sigil eq '@'
        {
        } else {
            $cond := Apply.new( code => Var.new(sigil=>'&',twigil=>'',name=>'prefix:<@>',namespace => [ 'GLOBAL' ],), arguments => [$cond] );
        }
        ' while (' ~ $.cond.emit_ruby ~ ').is_true6? ' ~ Main::newline()
        ~     $.body.emit_ruby
        ~ 'end'
        ~ Main::newline();
    }
}

class Decl {
    method emit_ruby {
        my $decl := $.decl;
        my $name := $.var.name;
        return $.var.emit_ruby;
        if $decl eq 'has' {
            # obsolete - "has" is handled by Visitor::MetaClass / Perl5::MOP
            return 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}';
        };
        my $create := ', \'new\', { modified => $_MODIFIED, name => \'' ~ $.var.emit_ruby ~ '\' } ) ';
        if $decl eq 'our' {
            my $s;
            # ??? use vars --> because compile-time scope is too tricky to use 'our'
            # ??? $s := 'use vars \'' ~ $.var.emit_ruby ~ '\'; ';
            $s := 'our ';

            if ($.var).sigil eq '$' {
                return $s
                    ~ $.var.emit_ruby
                    ~ ' = ::DISPATCH( $::Scalar' ~ $create
                    ~ ' unless defined ' ~ $.var.emit_ruby ~ '; '
                    ~ 'INIT { '
                    ~     $.var.emit_ruby
                    ~     ' = ::DISPATCH( $::Scalar' ~ $create
                    ~     ' unless defined ' ~ $.var.emit_ruby ~ '; '
                    ~ '}' ~ Main::newline()
            };
            if ($.var).sigil eq '&' {
                return $s
                    ~ $.var.emit_ruby
                    ~ ' = ::DISPATCH( $::Routine' ~ $create ~ ';' ~ Main::newline();
            };
            if ($.var).sigil eq '%' {
                return $s ~ $.var.emit_ruby
                    ~ ' = ::DISPATCH( $::HashContainer' ~ $create ~ ';' ~ Main::newline();
            };
            if ($.var).sigil eq '@' {
                return $s ~ $.var.emit_ruby
                    ~ ' = ::DISPATCH( $::ArrayContainer' ~ $create ~ ';' ~ Main::newline();
            };
            return $s ~ $.var.emit_ruby ~ Main::newline();
        };
        if ($.var).sigil eq '$' {
            return
                  $.decl ~ ' '
                # ~ $.type ~ ' '
                ~ $.var.emit_ruby ~ '; '
                ~ $.var.emit_ruby
                ~ ' = ::DISPATCH( $::Scalar' ~ $create
                ~ ' unless defined ' ~ $.var.emit_ruby ~ '; '
                ~ 'INIT { '
                ~     $.var.emit_ruby
                ~     ' = ::DISPATCH( $::Scalar' ~ $create
                ~ '}'
                ~ Main::newline()
                ;
        };
        if ($.var).sigil eq '&' {
            return
                  $.decl ~ ' '
                # ~ $.type ~ ' '
                ~ $.var.emit_ruby ~ '; '
                ~ $.var.emit_ruby
                ~ ' = ::DISPATCH( $::Routine' ~ $create
                ~ ' unless defined ' ~ $.var.emit_ruby ~ '; '
                ~ 'INIT { '
                ~     $.var.emit_ruby
                ~     ' = ::DISPATCH( $::Routine' ~ $create
                ~ '}'
                ~ Main::newline()
                ;
        };
        if ($.var).sigil eq '%' {
            return $.decl ~ ' '
                # ~ $.type
                ~ ' ' ~ $.var.emit_ruby
                ~ ' = ::DISPATCH( $::HashContainer' ~ $create ~ '; '
                ~ Main::newline();
        };
        if ($.var).sigil eq '@' {
            return $.decl ~ ' '
                # ~ $.type
                ~ ' ' ~ $.var.emit_ruby
                ~ ' = ::DISPATCH( $::ArrayContainer' ~ $create ~ '; '
                ~ Main::newline();
        };
        return $.decl ~ ' '
            # ~ $.type ~ ' '
            ~ $.var.emit_ruby;
    }
}

class Sig {
    method emit_ruby {
        my $inv := ' Undef.new';
        if $.invocant.isa( 'Var' ) {
            $inv := $.invocant.perl;
        }

        my $pos;
        my $decl;
        for @($.positional) -> $decl {
            $pos := $pos ~ $decl.emit_ruby ~ ', ';
        };

        my $named := '';  # TODO

          ' Signature.new( { '
        ~     'invocant: ' ~ $inv ~ ', '
        ~     'array:    List.new( [ ' ~ $pos   ~ ' ] ), '
        # ~     'hash     => ::DISPATCH( $::Hash,  "new", { _hash  => { ' ~ $named ~ ' } } ), '
        ~     'return: Undef.new '
        ~ '} )'
        ~ Main::newline();
    };
}

class Lit::Capture {
    method emit_ruby {
        my $s := ' c(';

	my $sa := '';
        if defined $.array {
           $sa := $sa ~ '[';
           my $item;
           for @.array -> $item {
                $sa := $sa ~ $item.emit_ruby ~ ', ';
           }
           $sa := $sa ~ ']';
	   $s := $s ~ $sa ~ ',';
        }
        else {
           $s := $s ~ 'nil,'
        };

	my $sh := '';
        if defined $.hash {
           $sh := $sh ~ '{';
           my $item;
           for @.hash -> $item {
               $sh := $sh ~ ' ' ~ ($item[0]).emit_ruby ~ ': ' ~ ($item[1]).emit_ruby ~ ', ';
           }
           $sh := $sh ~ '}';
	   $s := $s ~ $sh ~ ',';
        }
        else {
            $s := $s ~ 'nil,'
        };

        if defined $.invocant {
           $s := $s ~ $.invocant.emit_ruby ~ '';
        }
        else {
            $s := $s ~ 'nil'
        };

        return $s ~ ')' ~ Main::newline();
    };
}

class Lit::Subset {
    method emit_ruby {
          ' Subset.new({ '
        ~ 'base_class: ' ~ $.base_class.emit_ruby
        ~ ', '
        ~ 'block: '
        ~       '->(s__){ ' ~ ($.block.block).emit_ruby ~ ' }'    # XXX
        ~ ' } )' ~ Main::newline();
    }
}

class Method {
    method emit_ruby {
          '::DISPATCH( $::Code, \'new\', { '
        ~   'code => sub { '                 ~ Main::newline()
        ~     '# emit_declarations'          ~ Main::newline()
        ~     $.block.emit_declarations      ~ Main::newline()
        ~     '# get $self'                  ~ Main::newline()
        ~     '$self = shift; '              ~ Main::newline()
        ~     '# emit_arguments'             ~ Main::newline()
        ~     $.block.emit_arguments         ~ Main::newline()
        ~     '# emit_body'                  ~ Main::newline()
        ~     $.block.emit_body
        ~    ' }, '
        ~   'signature => '
        ~       $.block.emit_signature
        ~    ', '
        ~ ' } )'
        ~ Main::newline();
    }
}

class Sub {
    method emit_ruby {
          '::DISPATCH( $::Code, \'new\', { '
        ~   'code => sub { '
        ~       $.block.emit_declarations
        ~       $.block.emit_arguments
        ~       $.block.emit_body
        ~    ' }, '
        ~   'signature => '
        ~       $.block.emit_signature
        ~    ', '
        ~ ' } )'
        ~ Main::newline();
    }
}

class Macro {
    method emit_ruby {
        die 'Macros are not currently supported by the ruby backend.';
    }
}

class Do {
    method emit_ruby {
        Main::newline()
        ~ 'begin; '
        ~ $.block.emit_ruby
        ~ Main::newline()
        ~ 'end'
        ~ Main::newline();
    }
}

class BEGIN {
    method emit_ruby {
        ' ' ~
          $.block.emit_ruby ~
        ' '
    }
}

class Use {
    method emit_ruby {
        if ($.mod eq 'v6') {
            return Main::newline() ~ '#use v6' ~ Main::newline();
        }
        if ( $.perl5 ) {
	    die "ruby backend does not currently implement  use perl5";
        } else {
            return ('require '
                    ~ Main::singlequote() ~ $.mod ~ Main::singlequote()
		    ~ Main::newline());
        }
    }
}

=begin

=head1 NAME

KindaPerl6::Perl5::Emit::Ruby - Code generator for KindaPerl6-in-Perl5

=head1 DESCRIPTION

This module generates Perl 5 code for the KindaPerl6 compiler. This is
currently the primary and the most complete emitter. The runtime is
located in F<lib/KindaPerl6/Runtime/Perl5/>.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
