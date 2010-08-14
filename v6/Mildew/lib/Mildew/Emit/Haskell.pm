package Mildew::Emit::Haskell;
use Exporter qw(import);
use Scalar::Util qw(blessed);
our @EXPORT = qw(haskell_literal constructor);
sub haskell_literal {
    my ($thing) = @_;
    if (blessed $thing) {
        $thing->haskell_literal;
    } elsif (ref $thing eq 'ARRAY') {
        '['.join(',',map {haskell_literal($_)} @{$thing}).']';
    } elsif (not ref $thing) {
        '"' . $thing . '"';
    } else {
        die "don't know how to turn $thing to a haskell literal";
    }
}
sub constructor {
    my ($name,@args) = @_;
    '('.join(' ',$name,map {haskell_literal($_)} @args).')';
}
