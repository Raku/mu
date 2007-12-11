
use v6-alpha;

class KindaPerl6::Visitor::Emit::Perl6 {

    # This visitor is a perl6 emitter

    method visit ( $node ) {
        $node.emit_perl6;
    };

}

class CompUnit {
    method emit_perl6 {
          'module ' ~ $.name ~ " { "
        ~ Main::newline()
        ~ $.body.emit_perl6
        ~ Main::newline()
        ~ '};' ~ Main::newline();
    }
}

class Val::Int {
    method emit_perl6 {
        $.int
    }
}

class Val::Bit {
    method emit_perl6 {
        $.bit
    }
}

class Val::Num {
    method emit_perl6 {
        $.num
    }
}

class Val::Buf {
    method emit_perl6 {
        '\'' ~ $.buf ~ '\''
    }
}

class Val::Char {
    method emit_perl6 {
        '\'' ~ $.buf ~ '\''
    }
}

class Val::Undef {
    method emit_perl6 {
        '(undef)'
        #'GLOBAL::undef()'
    }
}

class Val::Object {
    method emit_perl6 {
        '::' ~ $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Native::Buf {
    method emit_perl6 {
        '\'' ~ $.buf ~ '\''
    }
}

class Lit::Seq {
    method emit_perl6 {
        '(' ~ (@.seq.>>emit_perl6).join(', ') ~ ')';
    }
}

class Lit::Array {
    method emit_perl6 {
        '[' ~ (@.array.>>emit_perl6).join(', ') ~ ']';
    }
}

class Lit::Hash {
    method emit_perl6 {
        my $fields := @.hash;
        my $str := '';
        my $field;
        for @$fields -> $field {
            $str := $str ~ ($field[0]).emit_perl6 ~ ' => ' ~ ($field[1]).emit_perl6 ~ ',';
        };
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    method emit_perl6 {
        my $s;
        my $name;
        for @($.pad.lexicals) -> $name {
            my $decl := ::Decl(
                decl => 'my',
                type => '',
                var  => ::Var(
                    sigil => '',
                    twigil => '',
                    name => $name,
                ),
            );
            $s := $s ~ $name.emit_perl6 ~ '; ' ~ Main::newline();
            #$s := $s ~ 'my ' ~ $name ~ '; ';
        };
        return
            $s
            ~ (@.body.>>emit_perl6).join('; ' ~ Main::newline() );
    }
}

class Lit::Object {
    method emit_perl6 {
        # $.class ~ '->new( ' ~ @.fields.>>emit_perl6.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit_perl6 ~ ' => ' ~ $_[1].emit_perl6}).join(', ') ~ ')';
        my $field;
        for @$fields -> $field {
            $str := $str ~ ($field[0]).emit_perl6 ~ ' => ' ~ ($field[1]).emit_perl6 ~ ',';
        };
        $.class ~ '.new( ' ~ $str ~ ' )';
    }
}

class Index {
    method emit_perl6 {
        $.obj.emit_perl6 ~ '[' ~ $.index.emit_perl6 ~ ']';
    }
}

class Lookup {
    method emit_perl6 {
        $.obj.emit_perl6 ~ '{' ~ $.index.emit_perl6 ~ '}';
    }
}

class Assign {
    method emit_perl6 {
        # TODO - same as ::Bind
        $.parameters.emit_perl6 ~ ' = ' ~ $.arguments.emit_perl6 ~ '';
    }
}

class Var {
    method emit_perl6 {
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
            return '$self->{' ~ $.name ~ '}'
        };

        if $.name eq '/' {
            return $table{$.sigil} ~ 'MATCH'
        };

        if $.sigil eq '&' {
            # debugging
            # return 'SIGIL(' ~ $.sigil ~ ') twigil(' ~ $.twigil ~ ') namespace(' ~ $.namespace.join( ':: ')  ~ ') name(' ~ $.name ~ ')'
            if $.namespace.join( '::' ) eq '' {
                # this is assumed to be a CORE:: routine, such as "say"
                return $.sigil ~ $.name
            } else {
                return $.sigil ~ $.twigil ~ $.namespace.join( '::' ) ~ $.name
            }
        };

        return Main::mangle_name( $.sigil, $.twigil, $.name );
    };
}

class Bind {
    method emit_perl6 {
        $.parameters.emit_perl6 ~ ' := ' ~ $.arguments.emit_perl6 ~ '';
        # $.arguments.emit_perl6;
    }
}

class Proto {
    method emit_perl6 {
        ~$.name
    }
}

class Call {
    method emit_perl6 {
        my $invocant;
        if $.invocant.isa( 'Str' ) {
            $invocant := '$::Class_' ~ $.invocant;
        }
        else {
        if $.invocant.isa( 'Val::Buf' ) {
            $invocant := '$::Class_' ~ $.invocant.buf;
        }
        else {
            $invocant := $.invocant.emit_perl6;
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
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_perl6).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_perl6).join(', ') ~ ')';
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';
        };

        my $call := (@.arguments.>>emit_perl6).join(', ');
        if ($.hyper) {
            # TODO - hyper + role
            '[ map { $_' ~ '->' ~ $meth ~ '(' ~ $call ~ ') } @{ ' ~ $invocant ~ ' } ]';
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
    method emit_perl6 {
        # WARNING: Putting white spaces in here, will mess up the subroutine calls
        return '(' ~ $.code.emit_perl6 ~ '(' ~ (@.arguments.>>emit_perl6).join(', ') ~ '))';
    }
}

class Return {
    method emit_perl6 {
        return
        'return (' ~ $.result.emit_perl6 ~ ')';
    }
}

class If {
    method emit_perl6 {
        # 'do { if ( ${' ~ $.cond.emit_perl6 ~ '} ) { ' ~ $.body.emit_perl6 ~ ' } '
        # the old code line above specified a scalar output directly via ${ }
        # I am not sure why the original author wanted that, but I believe it
        # is wrong.  However, I am not sure, so I am leaving this comment here.

        # if
        # WARNING: kp6 (not perl6) requires ( ) around the if conditionial
        'if ( ' ~ $.cond.emit_perl6 ~ ' ) '
        # then
        ~ '{ ' ~ Main::newline()
        ~ $.body.emit_perl6 ~ Main::newline()
        ~ '} '
        # else, if $.otherwise ? " else " : '';
        ~ ( $.otherwise ?? 'else { '
            ~ Main::newline()
            ~ $.otherwise.emit_perl6 ~ Main::newline()
            ~ '}' ~ Main::newline() !! ''
          )
    }
}

class Decl {
    method emit_perl6 {
        return $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_perl6;
    }
}

class Lit::SigArgument {
    method emit_perl6 {
        $.key.emit_perl6;
    }
}

class Sig {
    method emit_perl6 {
        (@.positional.>>emit_perl6).join(', ');
    }
}

class Method {
    method emit_perl6 {
        # TODO - signature binding
        my $sig := $.block.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant;
        # say $invocant.emit_perl6;

        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';   # no strict "vars"; ';

        # TODO - follow recursively
        my $pos := $sig.positional;
        my $field;
        for @$pos -> $field {
            $str := $str ~ 'my ' ~ $field.emit_perl6 ~ '; ' ~ Main::newline();
        };

        my $bind := ::Bind(
            'parameters' => ::Lit::Array( array => $sig.positional ),
            'arguments'  => ::Var( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit_perl6 ~ '; ' ~ Main::newline();

        'sub ' ~ $.name ~ ' { ' ~
          'my ' ~ $invocant.emit_perl6 ~ ' = shift; ' ~
          $str ~
          $.block.emit_perl6 ~
        ' }'
    }
}

class Sub {
    method emit_perl6 {
        'sub ' ~ $.name ~ ($.block.sig ?? '(' ~ ($.block.sig).emit_perl6 ~ ')' !! '') ~ ' { ' ~ Main::newline()
            ~ $.block.emit_perl6 ~ Main::newline()
            ~ ' }' ~ Main::newline();
    }
}

class Do {
    method emit_perl6 {
        'do { ' ~
          $.block.emit_perl6 ~
        ' }'
    }
}

class BEGIN {
    method emit_perl6 {
        'BEGIN { ' ~
          $.block.emit_perl6 ~
        ' }'
    }
}

class Use {
    method emit_perl6 {
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME

KindaPerl6::Visitor::Emit::Perl6 - Code generator for KindaPerl6

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
