
use v6-alpha;

module v6::AST;

sub node ( $match, $node_type ) {
    $match does $node_type
}
