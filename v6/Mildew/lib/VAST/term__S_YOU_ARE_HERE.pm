use v5.10;
use MooseX::Declare;
use utf8;
class VAST::term__S_YOU_ARE_HERE {
    use AST::Helpers;
    method emit_m0ld {
        # TODO handle the use of YOU_ARE_HERE outside of a setting
        AST::Assign->new(lvalue=>reg '$YOU_ARE_HERE',rvalue=>call clone=>reg '$scope');
    }
}
