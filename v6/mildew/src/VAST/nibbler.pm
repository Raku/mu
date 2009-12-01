use MooseX::Declare;
class VAST::nibbler {
    use Scalar::Util 'blessed';
    use AST::Helpers;
    use Data::Dumper;
    method emit_m0ld {
        my $str = '';
        for my $part (@{$self->{'.'}}) {
            if ($part->isa('VAST::Str')) {
                $str .= $part->{TEXT};
            } else {
                #die Dumper($part);
            }
        }
        return string $str;
    }
}

#sub as_constant_string {
#    my $m = shift;
#    my $str = '';
#    foreach my $nib (@{$m->{nibbles}}) {
#        if (blessed $nib) {
#            $str .= $nib->as_constant_string;
#        } else {
#            $str .= $nib;
#        }
#    }
#    return $str;
#}
#
#1;
