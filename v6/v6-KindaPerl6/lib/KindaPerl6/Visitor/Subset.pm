
use v6-alpha;

=begin

This visitor transforms subset declarations into constructors.

Before:

    my subset abc of xyz where { $_ == 123 };
    
After:
    
    my &abc := Subset.new( ... );

=end

class KindaPerl6::Visitor::Subset {

    method visit ( $node ) {

        # TODO - add 'our' subsets to the namespace
        
        # subset x {...}  -->  our $x := Subset.new( ... )
        if    ( $node.isa( 'Subset' ) )
           && ( $node.name ne '' )   # only named subsets ???
        {
            return ::Bind(  
                parameters => #::Decl(  
                    #decl  => 'our',  
                    #var   => 
                    ::Var(  
                        name   => $node.name,  
                        twigil => '',  
                        sigil  => '$', 
                    ),  
                    #type  => '', 
                #),  
                arguments => ::Call( 
                    'hyper'     => '',
                    'arguments' => [
                        # ::Val::Buf( buf => $node.name ),  
                    ],
                    'method'   => 'new',
                    'invocant' => ::Var(  
                        name   => 'Subset',  
                        twigil => '',  
                        sigil  => '$', 
                    ),  
                    # block => $node.block, 
                 ),
             );
        };

        # TODO !!!!

        # my sub x {...}  -->  my &x := sub {...}
        if    ( $node.isa( 'Apply' ) )
           && ( $node.code ).isa( 'Str' )
           && (  ( $node.code eq 'my' )
              || ( $node.code eq 'our' )
              )
           && ( (($node.arguments)[0]).isa( 'Subset' ) )
        {
            # my sub xxx 
            #  is parsed as:
            # my( sub xxx )
            return ::Bind(  
                parameters => ::Decl(  
                    decl  => $node.code,  
                    var   => ::Var(  
                        name   => (($node.arguments)[0]).name,  
                        twigil => '',  
                        sigil  => '&', 
                    ),  
                    type  => '', 
                ),  
                arguments => ::Sub( 
                    name  => '',  
                    block => (($node.arguments)[0]).block, 
                 ),
             );
        };

        return;
    };

}

