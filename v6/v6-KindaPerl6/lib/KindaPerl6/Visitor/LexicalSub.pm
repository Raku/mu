
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
        
        # TODO - my / our
        # TODO - Apply
        # TODO - add 'our' subs to the namespace, if there is one
        
        # only unnamed subs
        if    ( $node_name eq 'Sub' )
           && ( $data{'name'} ne '' ) 
        {
            # print $node_name, ' ', $node.name, '; ';
            return ::Bind(  
                parameters => ::Decl(  
                    decl  => 'our',  
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
