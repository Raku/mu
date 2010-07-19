use MooseX::Declare;
class VAST::infix__S_Comma {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        #XXX HACK
        map {$_ ? $_->emit_m0ld : ()} @{$self->{args}};
    }
}
