
use v6-alpha;

class KindaPerl6::Visitor::ShortCircuit {

    method visit ( $node, $node_name ) {
        if    ( ($node_name eq 'Apply') && (($node.code).name eq 'infix:<&&>') )
        {
            COMPILER::add_pad();
            my $pad := @COMPILER::PAD[0];
            COMPILER::drop_pad();
            return ::Apply(
                code => ::Var( name => 'infix:<&&>', twigil => '', sigil => '&', namespace => [ ] ),
                arguments => [
                ::Sub( 
                        block => ::Lit::Code(
                            pad => $pad,
                            body => [($node.arguments)[0]],
                            sig => ::Sig( positional => [ ], named => [ ] )
                        )
                ),
                ]
            );

        }
        return;
    };

}
