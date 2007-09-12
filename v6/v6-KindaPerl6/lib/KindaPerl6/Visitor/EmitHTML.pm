
use v6-alpha;

class KindaPerl6::Visitor::EmitHTML {

    # This visitor is a perl6 emitter
    
    method visit ( $node ) {
        html_header() ~ $node.emit_html ~ '</body></html>';
    };

    method css {
        my $nl := Main::newline();
        return '<style type="text/css">' ~ $nl
            ~ '.keyword { text-weight: bold; color: red; }' ~ $nl
            ~ '.builtin { color: red; }' ~ $nl
            ~ '.buffer { color: blue; }'~ $nl
            ~ '.variable { color: green; }'~ $nl
            ~ '.comp_unit { color: #555500; }' ~ $nl
            ~ '</style>' ~ $nl;
    };

    method html_header {
        my $nl := Main::newline();
        return '<html>' ~ $nl
            ~ '<head>' ~ $nl
            ~ '<title>Auto-Generated P6 Code</title>' ~ $nl
            ~ css()
            ~ '</head>' ~ $nl
            ~ '<body>' ~ $nl;
    }


}

class CompUnit {
    method emit_html {
          '{ <span class="keyword">module</span> ' ~ $.name ~ ';<br />' 
        ~ Main::newline()
        ~ $.body.emit_html
        ~ ' }</span><br />';
    }
}

class Val::Int {
    method emit_html { 
        '<span class="integer">' 
        ~ $.int 
        ~ '</span>'
    }
}

class Val::Bit {
    method emit_html { 
        $.bit 
    }
}

class Val::Num {
    method emit_html { 
        $.num 
    }
}

class Val::Buf {
    method emit_html { 
        '<span class="buffer">' 
        ~ '\'' ~ $.buf ~ '\'' 
        ~ '</span>'
    }
}

class Val::Undef {
    method emit_html { 
        '<span class="keyword">undef</span>'
        #'GLOBAL::undef()'
    }
}

class Val::Object {
    method emit_html {
        '::' ~ $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Native::Buf {
    method emit_html { 
        '\'' ~ $.buf ~ '\''
    }
}

class Lit::Seq {
    method emit_html {
        '(' ~ (@.seq.>>emit_html).join(', ') ~ ')';
    }
}

class Lit::Array {
    method emit_html {
        '[' ~ (@.array.>>emit_html).join(', ') ~ ']';
    }
}

class Lit::Hash {
    method emit_html {
        my $fields := @.hash;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit_html 
                ~ ' <span class="operator>=&gt;</span> ' 
                ~ ($field[1]).emit_html ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    method emit_html {
        my $s;
        for @($.pad.variable_names) -> $name {
            my $decl := ::Decl(
                decl => 'my',
                type => '',
                var  => ::Var(
                    sigil => '',
                    twigil => '',
                    name => $name,
                ),
            );
            $s := $s ~ $name.emit_html ~ '; <br />' ~ Main::newline();
            #$s := $s ~ 'my ' ~ $name ~ '; ';
        };
        return 
            $s
            ~ (@.body.>>emit_html).join('; <br />' ~ Main::newline());
    }
}

class Lit::Object {
    method emit_html {
        # $.class ~ '->new( ' ~ @.fields.>>emit_html.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit_html ~ ' => ' ~ $_[1].emit_html}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit_html ~ ' => ' ~ ($field[1]).emit_html ~ ',';
        }; 
        $.class ~ '.new( ' ~ $str ~ ' )';
    }
}

class Index {
    method emit_html {
        $.obj.emit_html ~ '[' ~ $.index.emit_html ~ ']';
    }
}

class Lookup {
    method emit_html {
        $.obj.emit_html ~ '{' ~ $.index.emit_html ~ '}';
    }
}

class Assign {
    method emit_html {
        # TODO - same as ::Bind
        $.parameters.emit_html ~ ' <span class="op">=</span> ' ~ $.arguments.emit_html ~ '';
    }
}

class Var {
    method emit_html {
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
            # XXX is the arrow in $self->{...} right?
            return '<span class="variable">$self->{' ~ $.name ~ '}</span>' 
        };
        
        if $.name eq '/' {
            return $table{$.sigil} ~ 'MATCH' 
        };
        
        return '<span class="variable">'
               ~ Main::mangle_name( $.sigil, $.twigil, $.name )
               ~ '</span>'; 
    };
}

class Bind {
    method emit_html {
        $.parameters.emit_html ~ ' <span class="operator>:=</span> ' ~ $.arguments.emit_html ~ '';
    }
}

class Proto {
    method emit_html {
        ~$.name        
    }
}

class Call {
    method emit_html {
        my $invocant;
        if $.invocant.isa( 'Str' ) {
            $invocant := '$::Class_' ~ $.invocant;
        }
        else {
        if $.invocant.isa( 'Val::Buf' ) {
            $invocant := '$::Class_' ~ $.invocant.buf;
        }
        else {
            $invocant := $.invocant.emit_html;
        };
        };
        if $invocant eq 'self' {
            $invocant := '$self';
        };
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
                return 
                    '[ <span class="keyword">map</span> { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_html).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_html).join(', ') ~ ')';
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';  
        };
        
        my $call := (@.arguments.>>emit_html).join(', ');
        if ($.hyper) {
            # TODO - hyper + role
            '[ <span class="map">map</span> { $_' ~ '->' ~ $meth ~ '(' ~ $call ~ ') } @{ ' ~ $invocant ~ ' } ]';
        }
        else {
               '('  ~ $invocant ~ '->FETCH->{_role_methods}{' ~ $meth ~ '}'
            ~ ' ?? ' ~ $invocant ~ '->FETCH->{_role_methods}{' ~ $meth ~ '}{code}' 
                ~ '(' ~ $invocant ~ '->FETCH, ' ~ $call ~ ')'
            ~ ' !! ' ~ $invocant ~ '->FETCH->' ~ $meth ~ '(' ~ $call ~ ')'
            ~  ')';
        };
        
    }
}

class Apply {
    method emit_html {
        return '(' ~ $.code.emit_html ~ ')(' ~ (@.arguments.>>emit_html).join(', ') ~ ')';
    }
}

class Return {
    method emit_html {
        return
            '<span class="keyword">return (' 
            ~ $.result.emit_html 
            ~ ")<br />"
            ~ Main::newline() ;
    }
}

class If {
    method emit_html {
        '<span class="keyword">do</span> { <span class="keyword">if</span> ( ${' ~ $.cond.emit_html ~ '->FETCH} ) { ' ~ $.body.emit_html ~ ' } '
        ~ ( $.otherwise 
            ?? ' else { ' ~ $.otherwise.emit_html ~ ' }' 
            !! '' 
          )
        ~ ' }';
    }
}

class For {
    method emit_html {
        my $cond := $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        '<span class="keyword">do</span> { <span class="keyword">for my</span> ' 
            ~ $.topic.emit_html ~ ' ( ' ~ $cond.emit_html ~ ' ) { ' ~ $.body.emit_html ~ ' } }';
    }
}

class Decl {
    method emit_html {
        return $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_html;
    }
}

class Sig {
    method emit_html {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
}

class Method {
    method emit_html {
        # TODO - signature binding
        my $sig := $.block.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant; 
        # say $invocant.emit_html;

        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';   # no strict "vars"; ';

        # TODO - follow recursively
        my $pos := $sig.positional;
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit_html ~ '; ';
        };

        my $bind := ::Bind( 
            'parameters' => ::Lit::Array( array => $sig.positional ), 
            'arguments'  => ::Var( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit_html ~ '; ';

        '<span class="keyword">sub</span> ' ~ $.name ~ ' { ' 
            ~ '<span class="keyword">my</span> ' 
            ~ $invocant.emit_html ~ ' = <span class="builtin">shift</span>; ' 
            ~ '<br />' ~ Main::newline() 
            ~ $str 
            ~ $.block.emit_html 
            ~ ' }'
    }
}

class Sub {
    method emit_html {
        # TODO - signature binding
        my $sig := $.block.sig;
        # say "Sig: ", $sig.perl;
        # say $invocant.emit_html;
        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';  # no strict "vars"; ;

        # TODO - follow recursively
        my $pos := $sig.positional;
        if @$pos {
            for @$pos -> $field { 
                $str := $str ~ 'my ' ~ $field.emit_html ~ '; ';
            };
    
            my $bind := ::Bind( 
                'parameters' => ::Lit::Array( array => $sig.positional ), 
                'arguments'  => ::Var( sigil => '@', twigil => '', name => '_' )
            );
            $str := $str ~ $bind.emit_html ~ '; ';
        };
        my $code :=
            'sub { '  
        ~      $str 
        ~      $.block.emit_html  
        ~    ' }';
        if $.name {
            return '$Code_' ~ $.name ~ ' :=  ' ~ $code ~ '';
        }
        return $code;
    }
}

class Do {
    method emit_html {
        '<span class="keyword">do</span> { <br />' ~ Main::newline() 
          ~ $.block.emit_html
          ~  "}<br />" ~ Main::newline();
    }
}

class BEGIN {
    method emit_html {
        '<span class="comp_unit">BEGIN</span> { <br />' ~ Main::newline() ~ 
          $.block.emit_html ~ 
        ' }<br />' ~ Main::newline();
    }
}

class Use {
    method emit_html {
        '<span class="comp_unit">use</span> ' ~ $.mod
    }
}

=begin

=head1 NAME 

KindaPerl6::Visitor::EmitPerl6 - Code generator for KindaPerl6

=head1 DESCRIPTION

This module generates Perl6 code for the KindaPerl6 compiler.

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
