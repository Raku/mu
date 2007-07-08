use v6-alpha;

class KindaPerl6::Traverse {

    sub visit ( $visitor, $node, $node_name ) {
        #say "visit " ~ $node ~ ' name ' ~ $node_name;
        
        if $node.isa('Array') {
            my $result := [ ];
            for @($node) -> $subitem {
                push @$result, visit_subnode( $visitor, $subitem ); 
            };
            return $result;
        };

        if $node.isa('Hash') {
            my $result := { };
            for keys %($node) -> $subitem {
                $result{ $subitem } := visit_subnode( $visitor, $node{$subitem} ); 
            };
            return $result;
        };

        if $node.isa('Str') {
            return $node;
        };

        if $node.isa('Pad') {
            return $node;
        };

        my $result := $visitor.visit( $node, $node_name );
        if ( $result ) {
            return $result;
        };
        
        my $result := { };
        my $data := $node.attribs;
        for keys %($data) -> $item {
            $result{$item} := visit_subnode( $visitor, $data{$item} );         
        };
        return $node.new(%$result);
        
    }

    sub visit_subnode ( $visitor, $subnode ) {
        if     $subnode.isa('Array') 
            || $subnode.isa('Hash') 
            || $subnode.isa('Str') 
            || $subnode.isa('Pad') 
        {
            return visit( $visitor, $subnode );
        }
        else {
            return $subnode.emit( $visitor );
        }
    }

}

#class Module {
#    has $.name;
#    has @.body;
#    method emit( $visitor ) {
#        KindaPerl6::Traverse::visit( 
#            $visitor, 
#            self,
#            'Module',
#        );
#    };
#    method attribs {
#            { 
#                name    => $.name,
#                body    => @.body,
#            }
#    };
#}

class CompUnit {
    has $.unit_type;
    has $.name;
    has @.traits;
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
                unit_type => $.unit_type,
                name    => $.name,
                traits  => @.traits,
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
    has %.pad;         #  is Mapping of Type; # All my/state/parameter variables
    has %.state;       #  is Mapping of Exp;  # State initializers, run upon first entry 
    has $.sig;         #  is Sig              # Signature
    has @.body;        #  is Seq of Exp;      # Code body 
    #has @.parameters;  #  is Seq of Exp;      # Signature
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Lit::Code',
        );
    };
    method attribs {
            { 
                pad   => %.pad,
                state => %.state,
                sig   => $.sig,
                body  => @.body,
            }
    };
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
    #has $.sig;
    has $.block;
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
                #sig     => $.sig,
                block   => $.block,
            }
    };
}

class Sub {
    has $.name;
    #has $.sig;
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
                #sig     => $.sig,
                block   => $.block,
            }
    };
}

class Token {
    has $.name;
    #has $.sig;
    has $.regex;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Token',
        );
    };
    method attribs {
            { 
                name    => $.name,
                #sig     => $.sig,
                regex   => $.regex,
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

class BEGIN {
    has @.block;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'BEGIN',
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


# ------------- REGEX AST ----------


class Rule {
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule',
        );
    };
    method attribs {
            { 
            }
    };
}

class Rule::Quantifier {
    has $.term;
    has $.quant;
    has $.greedy;
    has $.ws1;
    has $.ws2;
    has $.ws3;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Quantifier',
        );
    };
    method attribs {
            { 
                term   => $.term,
                quant  => $.quant,
                greedy => $.greedy,
                ws1    => $.ws1,
                ws2    => $.ws2,
                ws3    => $.ws3,
            }
    };
}

class Rule::Or {
    has @.or;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Or',
        );
    };
    method attribs {
            { 
                or   => $.or,
            }
    };
}

class Rule::Concat {
    has @.concat;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Concat',
        );
    };
    method attribs {
            { 
                concat => $.concat,
            }
    };
}

class Rule::Subrule {
    has $.metasyntax;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Subrule',
        );
    };
    method attribs {
            { 
                metasyntax   => $.metasyntax,
            }
    };
}

class Rule::SubruleNoCapture {
    has $.metasyntax;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::SubruleNoCapture',
        );
    };
    method attribs {
            { 
                metasyntax   => $.metasyntax,
            }
    };
}

class Rule::Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Var',
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

class Rule::Constant {
    has $.constant;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Constant',
        );
    };
    method attribs {
            { 
                constant   => $.constant,
            }
    };
}

class Rule::Dot {
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Dot',
        );
    };
    method attribs {
            { 
            }
    };
}

class Rule::SpecialChar {
    has $.char;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::SpecialChar',
        );
    };
    method attribs {
            { 
                char   => $.char,
            }
    };
}

class Rule::Block {
    has $.closure;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Block',
        );
    };
    method attribs {
            { 
                closure   => $.closure,
            }
    };
}

class Rule::InterpolateVar {
    has $.var;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::InterpolateVar',
        );
    };
    method attribs {
            { 
                var   => $.var,
            }
    };
}

class Rule::NamedCapture {
    has $.rule;
    has $.ident;
    has $.capture_to_array;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::NamedCapture',
        );
    };
    method attribs {
            { 
                rule   => $.rule,
                ident  => $.ident,
                capture_to_array => $.capture_to_array,
            }
    };
}

class Rule::Before {
    has $.rule;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Before',
        );
    };
    method attribs {
            { 
                rule   => $.rule,
            }
    };
}

class Rule::NotBefore {
    has $.rule;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::NotBefore',
        );
    };
    method attribs {
            { 
                rule   => $.rule,
            }
    };
}

class Rule::NegateCharClass {
    has $.chars;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::NegateCharClass',
        );
    };
    method attribs {
            { 
                chars   => $.chars,
            }
    };
}

class Rule::CharClass {
    has $.chars;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::CharClass',
        );
    };
    method attribs {
            { 
                chars   => $.chars,
            }
    };
}

class Rule::Capture {
    has $.rule;
    has $.position;
    has $.capture_to_array;
    method emit( $visitor ) {
        KindaPerl6::Traverse::visit( 
            $visitor, 
            self,
            'Rule::Capture',
        );
    };
    method attribs {
            { 
                rule     => $.rule,
                position => $.position,
                capture_to_array => $.capture_to_array,
            }
    };
}




=begin

=head1 NAME 

KindaPerl6::Traverse - Tree traverser for KindaPerl6 AST

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
