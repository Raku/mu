
use v6-alpha;

class KindaPerl6::Visitor::LexicalSub {

    # This visitor transforms subroutine declarations into variable declarations

    method visit ( $node, $node_name, $data ) {
        if $node_name eq 'CompUnit' {
            for @($node.body) -> $subitem {
                $subitem := $subitem.emit( self );
            };
            #return ::CompUnit(
            #    body  => $node.body;
            #);
        };
        if $node_name eq 'Sub' {
            # print $node_name, ' ', $node.name, '; ';
            return ::Bind(  
                parameters => ::Decl(  
                    decl  => 'my',  
                    var   => ::Var(  
                        name => '_SUB_' ~ $data{'name'},  
                        twigil => '',  
                        sigil => '$', 
                    ),  
                    type  => '', 
                ),  
                arguments => ::Sub( 
                    sig   => $data{'sig'},
                    name  => '',  
                    block => $data{'block'}, 
                 ),
             );
        };
        return $node;
    };

}
