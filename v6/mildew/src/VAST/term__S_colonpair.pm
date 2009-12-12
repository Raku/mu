use v5.10;
use MooseX::Declare;
use utf8;
class VAST::term__S_colonpair {
    use AST::Helpers;
    method emit_m0ld {
        $self->{colonpair}[0]->emit_m0ld;
    }
}
