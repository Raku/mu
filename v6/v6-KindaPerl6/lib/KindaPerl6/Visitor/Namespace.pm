
use v6-alpha;

=begin

This visitor maps global variables into a namespace-hash.

=end

class KindaPerl6::Visitor::Namespace {

    method visit ( $node, $node_name ) {
    
        if    ( $node_name eq 'Var' )
        {
            if @($node.namespace) {
                say "global ", $node.name;
            }
        };
        return;
    };

}
