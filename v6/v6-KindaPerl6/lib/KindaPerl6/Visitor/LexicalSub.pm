
use v6-alpha;

=begin

This visitor transforms subroutine declarations into variable declarations.

It allows the implementation of 'my' subroutines in the perl5 backend.

Before:

    my sub abc { 123 }
    abc();
    
After:
    
    my &abc := { 123 };
    &abc();

All non-my subroutines are declared as 'our'.

Before:

    our sub abc { 123 }
    abc();
    
After:
    
    sub abc { 123 }    # for perl5 compat
    our &abc := \&abc;
    &abc();

    # XXX - it is currently implemented like this, instead:
    our &abc := { 123 };
    &abc();

    # XXX - possible fix:
    our &abc := { 123 };
    sub abc { &abc( @_ ) }   # for perl5 compat
    &abc();

=end

# TODO
# - keep compatibility with normal perl5 code

class KindaPerl6::Visitor::LexicalSub {

    method visit ( $node ) {

        # TODO - add 'our' subs to the namespace
        
        # sub x {...}  -->  our &x := sub {...}
        if    ( $node.isa( 'Sub' ) )
           && ( $node.name ne '' )   # only named subs
        {
            return ::Bind(  
                parameters => ::Decl(  
                    decl  => 'our',  
                    var   => ::Var(  
                        name   => $node.name,  
                        twigil => '',  
                        sigil  => '&', 
                        namespace => [ ], 
                    ),  
                    type  => '', 
                ),  
                arguments => ::Sub( 
                    name  => '',  
                    block => $node.block, 
                 ),
             );
        };

        # my sub x {...}  -->  my &x := sub {...}
        if    ( $node.isa( 'Apply' ) )
           && ( $node.code ).isa( 'Str' )
           && (  ( $node.code eq 'my' )
              || ( $node.code eq 'our' )
              )
           && ( (($node.arguments)[0]).isa( 'Sub' ) )
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
                        namespace => [ ],
                    ),  
                    type  => '', 
                ),  
                arguments => ::Sub( 
                    name  => '',  
                    block => (($node.arguments)[0]).block, 
                 ),
             );
        };


        # x(...)  -->  &x(...)
        if    ( $node.isa( 'Apply' ) )
           && ( $node.code ).isa( 'Str' )
        {
            return ::Apply(  
                arguments => $node.arguments,
                code => ::Var(  
                        name   => $node.code,  
                        twigil => '',  
                        sigil  => '&', 
                        namespace => [ ],
                    ),   
             );
        };

        return;
    };

}

