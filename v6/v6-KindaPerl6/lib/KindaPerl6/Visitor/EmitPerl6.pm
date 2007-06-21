
use v6-alpha;

class KindaPerl6::Visitor::EmitPerl6 {

    # This visitor is a perl6 emitter
    
    method visit ( $node ) {
        $node.emit_perl6;
    };

}

class CompUnit {
    has $.unit_type;
    has $.name;
    has %.attributes;
    has %.methods;
    has $.body;
    method emit_perl6 {
          '{ module ' ~ $.name ~ "; " 
        ~ $.body.emit_perl6
        ~ ' }' ~ Main::newline();
    }
}

class Val::Int {
    has $.int;
    method emit_perl6 { 
        $.int 
    }
}

class Val::Bit {
    has $.bit;
    method emit_perl6 { 
        $.bit 
    }
}

class Val::Num {
    has $.num;
    method emit_perl6 { 
        $.num 
    }
}

class Val::Buf {
    has $.buf;
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
    has $.class;
    has %.fields;
    method emit_perl6 {
        '::' ~ $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Native::Buf {
    has $.buf;
    method emit_perl6 { 
        '\'' ~ $.buf ~ '\''
    }
}

class Lit::Seq {
    has @.seq;
    method emit_perl6 {
        '(' ~ (@.seq.>>emit_perl6).join(', ') ~ ')';
    }
}

class Lit::Array {
    has @.array;
    method emit_perl6 {
        '[' ~ (@.array.>>emit_perl6).join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash;
    method emit_perl6 {
        my $fields := @.hash;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit_perl6 ~ ' => ' ~ ($field[1]).emit_perl6 ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    has $.pad;   # see Pad.pm
    has $.state;
    has $.sig;
    has @.body;
    method emit_perl6 {
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
            $s := $s ~ $name.emit_perl6 ~ '; ';
            #$s := $s ~ 'my ' ~ $name ~ '; ';
        };
        return 
            $s
            ~ (@.body.>>emit_perl6).join('; ');
    }
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_perl6 {
        # $.class ~ '->new( ' ~ @.fields.>>emit_perl6.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit_perl6 ~ ' => ' ~ $_[1].emit_perl6}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit_perl6 ~ ' => ' ~ ($field[1]).emit_perl6 ~ ',';
        }; 
        $.class ~ '.new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit_perl6 {
        $.obj.emit_perl6 ~ '[' ~ $.index.emit_perl6 ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit_perl6 {
        $.obj.emit_perl6 ~ '{' ~ $.index.emit_perl6 ~ '}';
    }
}

class Assign {
    has $.parameters;
    has $.arguments;
    method emit_perl6 {
        # TODO - same as ::Bind
        $.parameters.emit_perl6 ~ ' = ' ~ $.arguments.emit_perl6 ~ '';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
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
        
        return Main::mangle_name( $.sigil, $.twigil, $.name ); 
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_perl6 {
        $.parameters.emit_perl6 ~ ' := ' ~ $.arguments.emit_perl6 ~ '';
    }
}

class Proto {
    has $.name;
    method emit_perl6 {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
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
    has $.code;
    has @.arguments;
    method emit_perl6 {
        return '(' ~ $.code.emit_perl6 ~ ')(' ~ (@.arguments.>>emit_perl6).join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit_perl6 {
        return
        'return(' ~ $.result.emit_perl6 ~ ')';
    }
}

class If {
    has $.cond;
    has $.body;
    has $.otherwise;
    method emit_perl6 {
        'do { if ( ${' ~ $.cond.emit_perl6 ~ '->FETCH} ) { ' ~ $.body.emit_perl6 ~ ' } '
        ~ ( $.otherwise 
            ?? ' else { ' ~ $.otherwise.emit_perl6 ~ ' }' 
            !! '' 
          )
        ~ ' }';
    }
}

class For {
    has $.cond;
    has $.body;
    has @.topic;
    method emit_perl6 {
        my $cond := $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { for my ' ~ $.topic.emit_perl6 ~ ' ( ' ~ $cond.emit_perl6 ~ ' ) { ' ~ $.body.emit_perl6 ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_perl6 {
        return $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_perl6;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_perl6 {
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
    #has $.sig;
    has $.block;
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
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit_perl6 ~ '; ';
        };

        my $bind := ::Bind( 
            'parameters' => ::Lit::Array( array => $sig.positional ), 
            'arguments'  => ::Var( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit_perl6 ~ '; ';

        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.emit_perl6 ~ ' = shift; ' ~
          $str ~
          $.block.emit_perl6 ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.block;
    method emit_perl6 {
        # TODO - signature binding
        my $sig := $.block.sig;
        # say "Sig: ", $sig.perl;
        # say $invocant.emit_perl6;
        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';  # no strict "vars"; ;

        # TODO - follow recursively
        my $pos := $sig.positional;
        if @$pos {
            for @$pos -> $field { 
                $str := $str ~ 'my ' ~ $field.emit_perl6 ~ '; ';
            };
    
            my $bind := ::Bind( 
                'parameters' => ::Lit::Array( array => $sig.positional ), 
                'arguments'  => ::Var( sigil => '@', twigil => '', name => '_' )
            );
            $str := $str ~ $bind.emit_perl6 ~ '; ';
        };
        my $code :=
            'sub { '  
        ~      $str 
        ~      $.block.emit_perl6  
        ~    ' }';
        if $.name {
            return '$Code_' ~ $.name ~ ' :=  ' ~ $code ~ '';
        }
        return $code;
    }
}

class Do {
    has $.block;
    method emit_perl6 {
        'do { ' ~ 
          $.block.emit_perl6 ~ 
        ' }'
    }
}

class BEGIN {
    has $.block;
    method emit_perl6 {
        'BEGIN { ' ~ 
          $.block.emit_perl6 ~ 
        ' }'
    }
}

class Use {
    has $.mod;
    method emit_perl6 {
        'use ' ~ $.mod
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
