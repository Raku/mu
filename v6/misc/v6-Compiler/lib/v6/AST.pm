
use v6-alpha;

module v6::AST;

# Note: v6.pm uses Pugs::Runtime::Perl6AST instead

sub node ( $match, $node_type ) {
    $match does $node_type
}
