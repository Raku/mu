
use v6-alpha;

class KindaPerl6::Visitor::LexicalSub {

    # This visitor transforms subroutine declarations into variable declarations

    method visit ( $node, $node_name, $data ) {
        if $node_name eq 'CompUnit' {
            for @($node.body) -> $subitem {
                $subitem.emit( self );
            };
            return;
        };
        if $node_name eq 'Sub' {
            print $node_name, ' ', $node.name, '; ';
        };
    };

}
