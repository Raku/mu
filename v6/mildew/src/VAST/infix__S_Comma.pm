use MooseX::Declare;
class VAST::infix__S_Comma {
    use AST::Helpers;
    method emit_m0ld {
        map {$_->emit_m0ld} @{$self->{args}};
    }
}
