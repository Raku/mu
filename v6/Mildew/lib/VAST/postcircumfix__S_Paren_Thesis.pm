use v5.10;
use MooseX::Declare;
use utf8;
class VAST::postcircumfix__S_Paren_Thesis {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        my $arg;
        if ($self->{arg}->isa('VAST::term__S_name')) {
            my @name = $self->{arg}{longname}->components;
            my $name = pop(@name);
            if (scalar @name) {
                $arg = call('postcircumfix:{ }'=>FETCH(lookup_package(@name)),[string('&'.$name)]);
            } else {
                $arg = lookup('&'.$name);
            }
        } else {
            $arg = $self->{arg}->emit_m0ld;
        }
        call 'postcircumfix:( )' => FETCH($arg),[capturize(named_and_positional($self->{postop}{postcircumfix}{semiarglist}->emit_m0ld))];
    }
}
