
use v6-alpha;

class KindaPerl6::Visitor::Emit::Nqp {

    # This visitor is a perl6 emitter

    method visit ( $node ) {
        $node.emit_nqp;
    };

}

class CompUnit {
    method emit_nqp {
          'module ' ~ $.name ~ ' {'
        ~ Main::newline()
        ~ $.body.emit_nqp
        ~ Main::newline()
        ~ '}'
	~ Main::newline();
    }
}

class Val::Int {
    method emit_nqp {
        $.int
    }
}

class Val::Bit {
    method emit_nqp {
        $.bit
    }
}

class Val::Num {
    method emit_nqp {
        $.num
    }
}

class Val::Buf {
    method emit_nqp {
        '\'' ~ $.buf ~ '\''
    }
}

class Val::Char {
    method emit_nqp {
        '\'' ~ $.buf ~ '\''
    }
}

class Val::Undef {
    method emit_nqp {
        '(undef)'
        #'GLOBAL::undef()'
    }
}

class Val::Object {
    method emit_nqp {
        '::' ~ $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Native::Buf {
    method emit_nqp {
        '\'' ~ $.buf ~ '\''
    }
}

class Lit::Seq {
    method emit_nqp {
        '(' ~ (@.seq.>>emit_nqp).join(', ') ~ ')';
    }
}

class Lit::Array {
    method emit_nqp {
        '[' ~ (@.array.>>emit_nqp).join(', ') ~ ']';
    }
}

class Lit::Hash {
    method emit_nqp {
        my $fields := @.hash;
        my $str := '';
        my $field;
        for @$fields -> $field {
            $str := $str ~ ($field[0]).emit_nqp ~ ' => ' ~ ($field[1]).emit_nqp ~ ',';
        };
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    method emit_nqp {
        my $s;
        my $name;
        for @($.pad.lexicals) -> $name {
            my $decl := Decl.new(
                decl => 'my',
                type => '',
                var  => Var.new(
                    sigil => '',
                    twigil => '',
                    name => $name,
                ),
            );
            $s := $s ~ $name.emit_nqp ~ '; ' ~ Main::newline();
            #$s := $s ~ 'my ' ~ $name ~ '; ';
        };
        return
            $s
            ~ (@.body.>>emit_nqp).join('; ' ~ Main::newline() );
    }
}

class Lit::Object {
    method emit_nqp {
        # $.class ~ '->new( ' ~ @.fields.>>emit_nqp.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit_nqp ~ ' => ' ~ $_[1].emit_nqp}).join(', ') ~ ')';
        my $field;
        for @$fields -> $field {
            $str := $str ~ ($field[0]).emit_nqp ~ ' => ' ~ ($field[1]).emit_nqp ~ ',';
        };
        $.class ~ '.new( ' ~ $str ~ ' )';
    }
}

class Index {
    method emit_nqp {
        $.obj.emit_nqp ~ '[' ~ $.index.emit_nqp ~ ']';
    }
}

class Lookup {
    method emit_nqp {
        $.obj.emit_nqp ~ '{' ~ $.index.emit_nqp ~ '}';
    }
}

class Assign {
    method emit_nqp {
        # TODO - same as ::Bind
        $.parameters.emit_nqp ~ ' = ' ~ $.arguments.emit_nqp ~ '';
    }
}

class Var {
    method emit_nqp {
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
    method emit_nqp {
        $.parameters.emit_nqp ~ ' := ' ~ $.arguments.emit_nqp ~ '';
        # $.arguments.emit_nqp;
    }
}

class Proto {
    method emit_nqp {
        ~$.name
    }
}

class Call {
    method emit_nqp {
        my $invocant;
        if $.invocant.isa( 'Str' ) {
            $invocant := '$::Class_' ~ $.invocant;
        }
        else {
        if $.invocant.isa( 'Val::Buf' ) {
            $invocant := '$::Class_' ~ $.invocant.buf;
        }
        else {
            $invocant := $.invocant.emit_nqp;
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
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_nqp).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_nqp).join(', ') ~ ')';
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';
        };

        my $call := (@.arguments.>>emit_nqp).join(', ');
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
    method emit_nqp {

        if ($.code).isa('Var') && (($.code.sigil) eq '&') {
            # remove the sigil
            return '(' 
                ~ $.code.twigil ~ ($.code.namespace).join( '::' ) ~ $.code.name
                ~ '(' ~ (@.arguments.>>emit_nqp).join(', ') ~ '))';
        }

        # WARNING: Putting white spaces in here, will mess up the subroutine calls
        return '(' ~ $.code.emit_nqp ~ '(' ~ (@.arguments.>>emit_nqp).join(', ') ~ '))';
    }
}

class Return {
    method emit_nqp {
        return
        'return (' ~ $.result.emit_nqp ~ ')';
    }
}

class If {
    method emit_nqp {
        # 'do { if ( ${' ~ $.cond.emit_nqp ~ '} ) { ' ~ $.body.emit_nqp ~ ' } '
        # the old code line above specified a scalar output directly via ${ }
        # I am not sure why the original author wanted that, but I believe it
        # is wrong.  However, I am not sure, so I am leaving this comment here.

        # if
        # WARNING: kp6 (not perl6) requires ( ) around the if conditionial
        'if ( ' ~ $.cond.emit_nqp ~ ' ) '
        # then
        ~ '{ ' ~ Main::newline()
        ~ $.body.emit_nqp ~ Main::newline()
        ~ '} '
        # else, if $.otherwise ? " else " : '';
        ~ ( $.otherwise ?? 'else { '
            ~ Main::newline()
            ~ $.otherwise.emit_nqp ~ Main::newline()
            ~ '}' ~ Main::newline() !! ''
          )
    }
}

class Decl {
    method emit_nqp {
        return $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_nqp;
    }
}

class Lit::SigArgument {
    method emit_nqp {
        $.key.emit_nqp;
    }
}

class Sig {
    method emit_nqp {
        (@.positional.>>emit_nqp).join(', ');
    }
}

class Method {
    method emit_nqp {
        # TODO - signature binding
        my $sig := $.block.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant;
        # say $invocant.emit_nqp;

        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';   # no strict "vars"; ';

        # TODO - follow recursively
        my $pos := $sig.positional;
        my $field;
        for @$pos -> $field {
            $str := $str ~ 'my ' ~ $field.emit_nqp ~ '; ' ~ Main::newline();
        };

        my $bind := Bind.new(
            'parameters' => Lit::Array.new( array => $sig.positional ),
            'arguments'  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit_nqp ~ '; ' ~ Main::newline();

        'sub ' ~ $.name ~ ' { ' ~
          'my ' ~ $invocant.emit_nqp ~ ' = shift; ' ~
          $str ~
          $.block.emit_nqp ~
        ' }'
    }
}

class Sub {
    method emit_nqp {
        'sub ' 
            #~ $.name 
            ~ ($.block.sig ?? '(' ~ ($.block.sig).emit_nqp ~ ')' !! '') ~ ' { ' ~ Main::newline()
            ~ $.block.emit_nqp ~ Main::newline()
            ~ ' }' ~ Main::newline();
    }
}

class Do {
    method emit_nqp {
        'do { ' ~
          $.block.emit_nqp ~
        ' }'
    }
}

class BEGIN {
    method emit_nqp {
        'BEGIN { ' ~
          $.block.emit_nqp ~
        ' }'
    }
}

class Use {
    method emit_nqp {
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
# vim: sw=4 ts=4 expandtab syntax=perl6
