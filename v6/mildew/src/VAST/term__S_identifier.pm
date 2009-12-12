use MooseX::Declare;
class VAST::term__S_identifier {
    use AST::Helpers;
    method emit_m0ld {
        my $func = lookup('&'.$self->{identifier}{TEXT});
        call 'postcircumfix:( )' => FETCH($func),[capturize(named_and_positional($self->{args}->emit_m0ld))];
    }
}
