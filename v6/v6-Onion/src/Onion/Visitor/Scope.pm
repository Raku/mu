
use v6-alpha;

=begin

This visitor maps lexical variables into a Scope object (see Runtime::Perl6::Scope).

=end

class KindaPerl6::Visitor::Scope {

    method visit ( $node, $node_name ) {
    
        # TODO !!!
        
        if    ( $node_name eq 'Lit::Code' )
        {

            return ::Lit::Code(
                pad   => $node.pad,
                state => $node.state,
                sig   => $node.sig,
                body  => [
            
                    # $MY = $MY.inner            
                    ::Assign(
                        parameters => ::Var(
                            namespace => [],
                            name      => 'MY',
                            twigil    => '',
                            sigil     => '$',
                        ),
                        arguments => ::Call(
                            hyper     => undef,
                            arguments => undef,
                            method    => 'inner',
                            invocant  => ::Var(
                                namespace => [],
                                name      => 'MY',
                                twigil    => '',
                                sigil     => '$',
                            ),
                        ),
                    ),

                    @($node.body),

                    # $MY = $MY.outer            
                    ::Assign(
                        parameters => ::Var(
                            namespace => [],
                            name      => 'MY',
                            twigil    => '',
                            sigil     => '$',
                        ),
                        arguments => ::Call(
                            hyper     => undef,
                            arguments => undef,
                            method    => 'outer',
                            invocant  => ::Var(
                                namespace => [],
                                name      => 'MY',
                                twigil    => '',
                                sigil     => '$',
                            ),
                        ),
                    ),

                ] 
            );  
        };
        return;
    };

}
