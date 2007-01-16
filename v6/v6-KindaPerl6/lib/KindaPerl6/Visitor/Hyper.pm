
use v6-alpha;

=begin

This visitor transforms hyper method calls into map() calls.

It allows the implementation of '.>>' in the perl5 and parrot backends.

Before:

    @abc.>>x()
    
After:
    
    map { .x() }, @abc

=end

class KindaPerl6::Visitor::Hyper {

    method visit ( $node, $node_name ) {
        if    ( $node_name eq 'Call' )
           && ( $node.hyper )
        {
            return ::Apply(
                code      => 'map',
                arguments => [
                    ::Sub( 
                        sig   => ::Sig( positional => [ ], named => [ ] ),  
                        block => [
                            ::Call( 
                                invocant  => ::Var( sigil => '$', twigil => '', name => '_' ),
                                method    => $node.method,
                                arguments => $node.arguments,
                            ),
                        ]
                    ),
                    $node.invocant,
                ]
            );
        };
        return;
    };

}
