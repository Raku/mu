
use v6-alpha;

class KindaPerl6::Visitor::ShortCircuit {
    sub new_pad {
        COMPILER::add_pad();
        my $pad := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        return $pad;
    };
    sub thunk($value) {
        ::Sub( 
                block => ::Lit::Code(
                    pad => new_pad(),
                    body => [$value],
                    sig => ::Sig( positional => [ ], named => [ ] )
                )
        );
    };
    method visit ( $node, $node_name ) {
        my $pass_thunks := {'infix:<&&>'=>1,'infix:<||>'=>1,'infix:<//>'=>1};
        if ($node_name eq 'Apply') && $pass_thunks{($node.code).name}
        {
            my $left := (($node.arguments)[0]).emit(self);
            my $right := (($node.arguments)[1]).emit(self);
            return ::Apply(
                code => ::Var( name => ($node.code).name , twigil => '', sigil => '&', namespace => [ ] ),
                arguments => [ thunk($left),thunk($right) ]
            );

        }
        return;
    };

}
