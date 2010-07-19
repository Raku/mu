use v5.10;
use MooseX::Declare;
use utf8;
# CONTROL blocks are moved to the top of the
# statementlist, so we know that no code was executed
# before this, so we can peacefully delay the setup of the
# control block up to this point.
class VAST::statement_control__S_CONTROL {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        call 'set_control' => (call 'continuation' => reg '$interpreter'), [ code($self->{block},FETCH(lookup('$DefaultBlockSignature'))) ];
    }
}
