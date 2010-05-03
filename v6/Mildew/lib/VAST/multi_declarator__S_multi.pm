package VAST::multi_declarator__S_multi;
use utf8;
use strict;
use warnings;
use AST::Helpers;
use v5.10;

sub emit_m0ld {
    my ($m) = @_;
    my $routine = $m->{routine_def};
    my $name;
    if ($routine->{deflongname}[0]) {
        $name = '&'.$routine->{deflongname}[0]{name}{identifier}{TEXT};
        if (my $colonpair = $routine->{deflongname}[0]{colonpair}[0]) {
            if ($colonpair->{coloncircumfix}{circumfix}->isa('VAST::circumfix__S_Lt_Gt')) {
                $name .= ':'.$colonpair->{coloncircumfix}{circumfix}->{nibble}->Str;
            } else {
                XXX;
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
