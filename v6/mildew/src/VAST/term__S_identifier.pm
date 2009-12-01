use MooseX::Declare;
class VAST::term__S_identifier {
    use AST::Helpers;
    method emit_m0ld {
        my $func = lookup('&'.$self->{identifier}{TEXT});
        my @args = $self->{args}->emit_m0ld;
        my @positional = grep { ref $_ ne 'AST::Pair' } @args;
        my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
        call 'postcircumfix:( )' => FETCH($func),[capturize(\@positional,\@named)];
    }
}
