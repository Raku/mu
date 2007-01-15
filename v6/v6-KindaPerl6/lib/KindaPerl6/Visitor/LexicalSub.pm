
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
        
        if    ( $node_name eq 'Sub' )
           && ( $data{'name'} ne '' )   # only named subs
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


        if    ( $node_name eq 'Apply' )
           && ($data{'code'}).isa( 'Str' )
           && (  ( $data{'code'} eq 'my' )
              || ( $data{'code'} eq 'our' )
              )
           && ( (($data{'arguments'})[0]).isa( 'Sub' ) )
        {
            # my sub xxx 
            #  is parsed as:
            # my( sub xxx )
            return ::Bind(  
                parameters => ::Decl(  
                    decl  => $data{'code'},  
                    var   => ::Var(  
                        name => '_SUB_' ~ (($data{'arguments'})[0]).name,  
                        twigil => '',  
                        sigil => '$', 
                    ),  
                    type  => '', 
                ),  
                arguments => ::Sub( 
                    sig   => (($data{'arguments'})[0]).sig,
                    name  => '',  
                    block => (($data{'arguments'})[0]).block, 
                 ),
             );
        };


        if    ( $node_name eq 'Apply' )
           && ($data{'code'}).isa( 'Str' )
        {
            return ::Apply(  
                arguments => $data{'arguments'},
                code => ::Var(  
                        name => '_SUB_' ~ $data{'code'},  
                        twigil => '',  
                        sigil => '$', 
                    ),   
             );
        };

        return $node;
    };

}

