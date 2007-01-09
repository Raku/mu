use v6-alpha;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Int {
    has $.int;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Bit {
    has $.bit;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Num {
    has $.num;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Buf {
    has $.buf;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Undef {
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Lit::Seq {
    has @.seq;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Lit::Array {
    has @.array;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Lit::Hash {
    has @.hash;
    method emit( $visitor ) {
        $visitor( self );
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
        $visitor( self );
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Proto {
    has $.name;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Return {
    has $.result;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Do {
    has @.block;
    method emit( $visitor ) {
        $visitor( self );
    }
}

class Use {
    has $.mod;
    method emit( $visitor ) {
        $visitor( self );
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
