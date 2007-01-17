
use v6-alpha;

=begin

This visitor transforms subroutine declarations into variable declarations.

It allows the implementation of 'my' subroutines in the perl5 backend.

Before:

    my sub abc { 123 }
    abc();
    
After:
    
    my $_SUB_abc := { 123 };
    $_SUB_abc();

All non-my subroutines are declared as 'our'.

Before:

    our sub abc { 123 }
    abc();
    
After:
    
    sub abc { 123 }    # for perl5 compat
    our $_SUB_abc := \&abc;
    $_SUB_abc();

    # XXX - it is currently implemented like this, instead:
    our $_SUB_abc := { 123 };
    $_SUB_abc();

    # XXX - possible fix:
    our $_SUB_abc := { 123 };
    sub abc { $_SUB_abc( @_ ) }   # for perl5 compat
    $_SUB_abc();

=end

class KindaPerl6::Visitor::LexicalSub {

    method visit ( $node, $node_name ) {

        # TODO - add 'our' subs to the namespace, if there is one

        if ( $node_name eq 'Lit::Code' ) {
            # TODO - collect lexicals into $node.pad  
        };
        
        my $data := $node.attribs;
        
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

        return;
    };

}

