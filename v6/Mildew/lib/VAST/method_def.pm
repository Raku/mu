package VAST::method_def;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    let FETCH(lookup('$?CLASS')), sub {
        my $CLASS = shift;
        use YAML::XS;

        my $sig = $m->{multisig}[0]{signature}[0];
        call add_method => FETCH(call '^!how' => $CLASS),[$CLASS,string $m->{longname}->canonical, routine($m->{blockoid},($sig ? $sig->emit_m0ld_with_invocant : FETCH(lookup('$DefaultMethodSignature'))))];
    };
}

1;
