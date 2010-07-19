use MooseX::Declare;
class VAST::nibbler {
    use Scalar::Util 'blessed';
    use Mildew::AST::Helpers;
    method emit_m0ld {
        my @parts = @{$self->{'.'}};
        if (@parts == 1) {
            $parts[0]->emit_m0ld;
        } elsif (@parts == 0) {
            string '';
        } else {
            fcall '&infix:~',[map {$_->emit_m0ld} @parts];
        }
    }
}

