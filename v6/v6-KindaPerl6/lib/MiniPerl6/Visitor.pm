use v6-alpha;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit( $visitor ) {
        $visitor.visit( 
            'CompUnit',
            { 
                name    => $.name,
                attributes => %.attributes,
                methods => %.methods,
                body    => @.body,
            }
        );
    }
}

class Val::Int {
    has $.int;
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Int',
            { 
                int    => $.int,
            }
        );
    }
}

class Val::Bit {
    has $.bit;
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Bit',
            { 
                bit    => $.bit,
            }
        );
    }
}

class Val::Num {
    has $.num;
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Num',
            { 
                num    => $.num,
            }
        );
    }
}

class Val::Buf {
    has $.buf;
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Buf',
            { 
                buf    => $.buf,
            }
        );
    }
}

class Val::Undef {
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Undef',
            { 
            }
        );
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit( $visitor ) {
        $visitor.visit( 
            'Val::Object',
            { 
                class  => $.class,
                fields => %.fields,
            }
        );
    }
}

class Lit::Seq {
    has @.seq;
    method emit( $visitor ) {
        $visitor.visit( 
            'Lit::Seq',
            { 
                seq  => @.seq,
            }
        );
    }
}

class Lit::Array {
    has @.array;    
    method emit( $visitor ) {
        $visitor.visit( 
            'Lit::Array',
            { 
                array  => @.array,
            }
        );
    }
}

class Lit::Hash {
    has @.hash;
    method emit( $visitor ) {
        $visitor.visit( 
            'Lit::Hash',
            { 
                hash  => @.hash,
            }
        );
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit( $visitor ) {
        $visitor.visit( 
            'Lit::Object',
            { 
                class  => $.class,
                fields => %.fields,
            }
        );
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        $visitor.visit( 
            'Index',
            { 
                obj   => $.obj,
                index => $.index,
            }
        );
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        $visitor.visit( 
            'Lookup',
            { 
                obj   => $.obj,
                index => $.index,
            }
        );
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit( $visitor ) {
        $visitor.visit( 
            'Var',
            { 
                sigil   => $.sigil,
                twigil  => $.twigil,
                name    => $.name,
            }
        );
    }
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit( $visitor ) {
        $visitor.visit( 
            'Bind',
            { 
                parameters   => $.parameters,
                arguments    => $.arguments,
            }
        );
    }
}

class Proto {
    has $.name;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Return {
    has $.result;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Do {
    has @.block;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

class Use {
    has $.mod;
    method emit( $visitor ) {
        $visitor.visit( self );
    }
}

=begin

=head1 NAME 

MiniPerl6::Visitor - Tree traverser for MiniPerl6 AST

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
