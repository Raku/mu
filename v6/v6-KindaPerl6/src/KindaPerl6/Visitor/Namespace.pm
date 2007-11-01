
use v6-alpha;

=begin

This visitor maps global variables into a namespace-hash.

=end

class KindaPerl6::Visitor::Namespace {

    method visit ( $node, $node_name ) {
    
        if    ( $node_name eq 'Var' )
        {
            if @($node.namespace) {
                #say "global ", $node.name;
                # $X::Y::z -> %KP6<X::Y><Scalar_z>
                return ::Call(
                     'invocant' => ::Call(
                             'invocant' => ::Var(
                                    namespace => [ 'GLOBAL' ],
                                    name      => 'KP6',
                                    twigil    => '',
                                    sigil     => '%',
                                ),
                             'arguments' => [ ::Val::Buf( buf => ($node.namespace).join('::') ) ],
                             'method' => 'LOOKUP',
                             'hyper' => ''
                        ),
                     'arguments' => [ ::Val::Buf( buf => ( $node.sigil ~ $node.name) ) ],
                     'method' => 'LOOKUP',
                     'hyper' => ''
                );
            }
        };
        return;
    };

}
