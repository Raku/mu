use v5.10;
use MooseX::Declare;
class VAST::infix__S_AmpAmp {
    use AST::Helpers;
    method emit_m0ld {
        let $self->{args}[0]->emit_m0ld,sub {
            my $left = shift; 
            AST::If->new(cond => $left,then => $self->{args}[1]->emit_m0ld,else => $left);
        };
    }
}
