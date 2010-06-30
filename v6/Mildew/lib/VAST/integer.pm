use MooseX::Declare;
class VAST::integer {
    use AST::Helpers;
    method emit_m0ld {
        #XXX non-base 10
        integer($self->{decint}{TEXT});
    }
}
