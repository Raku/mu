use MooseX::Declare;
use utf8;
class VAST::term__S_identifier {
    use AST::Helpers;
    method emit_m0ld {
        my $id = $self->{identifier}{TEXT};
        if ($id eq 'INFERRED-TYPE-TEST') {
            use Data::Dumper;
            my @args = $self->{args}->emit_m0ld;
            if ($args[1]->isa('AST::StringConstant')) {
                fcall '&ok' => [AST::InferredTypeTest->new(value=>$args[0],test=>$args[1]->value)]; 
            } else {
                die 'the test argument to INFERED-TYPE-TEST must be a string constant';
            }
        } else {
            fcall '&'.$id => named_and_positional($self->{args}->emit_m0ld);
        }
    }
}
