use v6-alpha;

class KindaPerl6::Traverse {

    sub visit ( $visitor, $node, $node_name ) {
        # say "visit " ~ $node_name;
        
        if $node.isa('Array') {
            my $result := [ ];
            my $i := 0;
            for @($node) -> $subitem {
                $result[ $i ] := $subitem.emit( $visitor );
                $i := $i + 1;
            };
            return $result;
        };

        if $node.isa('Hash') {
            my $result := { };
            for keys %($node) -> $subitem {
                $result{ $subitem } := ($node{$subitem}).emit( $visitor );
            };
            return $result;
        };

        if $node.isa('Str') {
            return $node;
        };

        my $result := $visitor.visit( $node, $node_name );
        if ( $result ) {
            return $result;
        };
        
        my $result := { };
        my $data := $node.attribs;
        for keys %($data) -> $item {            
            $result{$item} := visit( 
                $visitor, 
                $data{$item}
            );
        };
        return $node.new(%$result);
        
    };

}

class Module {
    has $.name;
    has @.body;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Module',
        );
    };
    method attribs {
            { 
                name    => $.name,
                body    => @.body,
            }
    };
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'CompUnit',
        );
    };
    method attribs {
            { 
                name    => $.name,
                attributes => %.attributes,
                methods => %.methods,
                body    => @.body,
            }
    };
}

class Val::Int {
    has $.int;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Int',
        );
    };
    method attribs {
            { 
                int    => $.int,
            }
    };
}

class Val::Bit {
    has $.bit;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Bit',
        );
    };
    method attribs {
            { 
                bit    => $.bit,
            }
    };
}

class Val::Num {
    has $.num;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Num',
        );
    };
    method attribs {
            { 
                num    => $.num,
            }
    };
}

class Val::Buf {
    has $.buf;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Buf',
        );
    };
    method attribs {
            { 
                buf    => $.buf,
            }
    };
}

class Val::Undef {
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Undef',
        );
    };
    method attribs {
            { 
            }
    };
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Val::Object',
        );
    };
    method attribs {
            { 
                class  => $.class,
                fields => %.fields,
            }
    };
}

class Lit::Seq {
    has @.seq;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lit::Seq',
        );
    };
    method attribs {
            { 
                seq  => @.seq,
            }
    };
}

class Lit::Array {
    has @.array;    
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lit::Array',
        );
    };
    method attribs {
            { 
                array  => @.array,
            }
    };
}

class Lit::Hash {
    has @.hash;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lit::Hash',
        );
    };
    method attribs {
            { 
                hash  => @.hash,
            }
    };
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lit::Object',
        );
    };
    method attribs {
            { 
                class  => $.class,
                fields => %.fields,
            }
    };
}

class Index {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Index',
        );
    };
    method attribs {
            { 
                obj   => $.obj,
                index => $.index,
            }
    };
}

class Lookup {
    has $.obj;
    has $.index;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lookup',
        );
    };
    method attribs {
            { 
                obj   => $.obj,
                index => $.index,
            }
    };
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Var',
        );
    };
    method attribs {
            { 
                sigil   => $.sigil,
                twigil  => $.twigil,
                name    => $.name,
            }
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Bind',
        );
    };
    method attribs {
            { 
                parameters   => $.parameters,
                arguments    => $.arguments,
            }
    };
}

class Assign {
    has $.parameters;
    has $.arguments;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Assign',
        );
    };
    method attribs {
            { 
                parameters   => $.parameters,
                arguments    => $.arguments,
            }
    };
}

class Proto {
    has $.name;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Proto',
        );
    };
    method attribs {
            { 
                name   => $.name,
            }
    };
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Call',
        );
    };
    method attribs {
            { 
                invocant   => $.invocant,
                hyper      => $.hyper,
                method     => $.method,
                arguments  => @.arguments,
            }
    };
}

class Apply {
    has $.code;
    has @.arguments;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Apply',
        );
    };
    method attribs {
            { 
                code       => $.code,
                arguments  => @.arguments,
            }
    };
}

class Return {
    has $.result;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Return',
        );
    };
    method attribs {
            { 
                result    => $.result,
            }
    };
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'If',
        );
    };
    method attribs {
            { 
                cond       => $.cond,
                body       => @.body,
                otherwise  => @.otherwise,
            }
    };
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'For',
        );
    };
    method attribs {
            { 
                cond       => $.cond,
                body       => @.body,
                topic      => @.topic,
            }
    };
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Decl',
        );
    };
    method attribs {
            { 
                decl       => $.decl,
                type       => @.type,
                var        => @.var,
            }
    };
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Sig',
        );
    };
    method attribs {
            { 
                invocant   => $.invocant,
                positional => @.positional,
                named      => @.named,
            }
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Method',
        );
    };
    method attribs {
            { 
                name    => $.name,
                sig     => $.sig,
                block   => @.block,
            }
    };
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Sub',
        );
    };
    method attribs {
            { 
                name    => $.name,
                sig     => $.sig,
                block   => @.block,
            }
    };
}

class Do {
    has @.block;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Do',
        );
    };
    method attribs {
            { 
                block   => @.block,
            }
    };
}

class Use {
    has $.mod;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Use',
        );
    };
    method attribs {
            { 
                mod    => $.mod,
            }
    };
}

=begin

=head1 NAME 

MiniPerl6::Traverse - Tree traverser for MiniPerl6 AST

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
