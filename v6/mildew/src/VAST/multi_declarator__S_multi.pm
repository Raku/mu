package VAST::multi_declarator__S_multi;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m) = @_;
    my $routine = $m->{routine_def};
    my $name;
    if ($routine->{deflongname}[0]) {
        $name = '&'.$routine->{deflongname}[0]{name}{identifier}{TEXT};
        if (my $colonpair = $routine->{deflongname}[0]{colonpair}[0]) {
            if ($colonpair->{v}{sym}[0] eq '<' &&
                 $colonpair->{v}{sym}[1] eq '>') {
                $name .= ':'.$colonpair->{v}{nibble}->as_constant_string;
            } else {
            }
        }
    }

    my $sig = $routine->{multisig}[0]{signature}[0];
    my $rout = routine($routine->{blockoid},($sig ? $sig->emit_m0ld : empty_sig));
    my $ret = AST::Seq->new(stmts=>[
        $Mildew::multis->{$name} ? () : call(BIND => curlies($name),[call new => lookupf('Multi')]),
        call(get_outer_candidates => lookupf($name),[string $name,reg '$scope']),
        call(push => FETCH(call candidates => lookupf($name)),[$rout]),
    ]);
    $Mildew::multis->{$name} = 1;
    return $ret;
}

1;
