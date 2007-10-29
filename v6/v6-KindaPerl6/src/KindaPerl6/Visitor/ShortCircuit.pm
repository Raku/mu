
use v6-alpha;

class KindaPerl6::Visitor::ShortCircuit {
    sub thunk($value,$pad) {
        ::Sub( 
                block => ::Lit::Code(
                    pad => COMPILER::inner_pad($KindaPerl6::Visitor::ShortCircuit::last_pad),
                    body => [$value],
                    sig => ::Sig( invocant => undef, positional => [ ] )
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
                code => $node.code,
                arguments => [ thunk($left),thunk($right) ]
            );

        }
        if    ( $node_name eq 'Lit::Code' )
        {
            $KindaPerl6::Visitor::ShortCircuit::last_pad := $node.pad;
        }
        return;
    };

}
