
use v6-alpha;

=begin

This visitor looks up lexical variables, and adds a GLOBAL lookup if needed.

=end

class KindaPerl6::Visitor::Global {

    has $.pad;

    method visit ( $node, $node_name ) {
    
        #say "Global ",$node_name;
    
        if    ( $node_name eq 'CompUnit' )
        {
            ($node.body).emit( $self );
            return $node;
        }
        
        if    ( $node_name eq 'Lit::Code' )
        {
            unshift @($.pad), $node.pad;
            for @($node.body) -> $stmt {
                $stmt.emit( $self );
            };
            shift @($.pad);
            return $node;
        }
        
        if    ( $node_name eq 'Var' )
        {
            #say "MetaClass - Class: ", $node.name ," Lexicals: ";
            #my $lexicals := (($node.body).pad).lexicals;
            #for @$lexicals -> $var { say $var; }
            #my $var := ((@$lexicals)[0]).var;
        
            #say "variable: ", $node.sigil, $node.twigil, $node.name;
            #say "pad: ", $.pad.perl;

            if (($.pad)[0]).declaration( $node ) {
                # say "ok - declaration ", $node.name;
            }
            else {
            
                # TODO - lookup into the GLOBAL namespace
            
                warn "undeclared variable: ", $node.sigil, $node.twigil, $node.name;
            }
            return $node;                    
        };
        return;
    };

}
