
use v6-alpha;
use v6::AST::Native;

module v6::Emitter::Native::Perl;

multi emit ( v6::AST::NStr $node ) {
    "'" ~ $node.str ~ "'"
}

multi emit ( v6::AST::Native $node ) {
    $node.str
}
