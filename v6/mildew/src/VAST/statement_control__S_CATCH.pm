use v5.10;
use MooseX::Declare;
use utf8;
# CATCH blocks are moved to the top of the
# statementlist, so we know that no code was executed
# before this, so we can peacefully delay the setup of the
# catch block up to this point.
class VAST::statement_control__S_CATCH {
    use AST::Helpers;
    method emit_m0ld {
        call 'set_catch' => (call 'continuation' => reg '$interpreter'), [ code($self->{block},FETCH(lookup('$DefaultBlockSignature'))) ];
    }
}
