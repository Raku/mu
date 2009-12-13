use v5.10;
use MooseX::Declare;
use utf8;
class VAST::Str {
    use AST::Helpers;
    method emit_m0ld {
        string $self->{TEXT};
    }
}
