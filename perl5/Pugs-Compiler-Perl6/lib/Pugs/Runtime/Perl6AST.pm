
# file Pugs::Runtime::Perl6AST
use v6-alpha;

module v6::AST;

$INC{"v6/AST.pm"}=1;  # mark v6::AST as "used"

sub node( $match, $type ) {
    #say "new node $match $type\n";
    $match;
}
