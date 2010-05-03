use v5.10;
use MooseX::Declare;
use utf8;
class VAST::escape__S_Back {
    use AST::Helpers;
    method emit_m0ld {
        my %backslashes = (n=>"\n");
        if ($self->{item}->isa('VAST::backslash__S_stopper')) {
            return string $self->{item}{text}{TEXT};
        }
        if ($self->{item}->isa('VAST::backslash__S_Back')) {
            return string '\\';
        }
        if ($self->{item}{text}) {
            return string $self->{item}{text};
        }
        
        my $TEXT = $self->{item}{TEXT};
        YYY($self) unless $backslashes{$TEXT};
        string $backslashes{$TEXT};
    }
}
