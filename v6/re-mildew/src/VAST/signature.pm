package VAST::signature;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld_ahsig_with_invocant {
    my $m = shift;
    my @stmts;
    push @stmts, call BIND => (call 'postcircumfix:{ }' => reg '$scope',[string '$¿self']),[call new => lookupf('Scalar'),[call positional => reg '$capture',[integer 0]]];

    $m->emit_m0ld_ahsig(1,@stmts);
}
sub emit_m0ld_ahsig {
    my $m = shift;

    my $other = shift || 0;
    my @stmts = @_;

    # TODO invocant

    for my $param (@{$m->{parameter}}) {
        push @stmts, $param->emit_m0ld_ahsig_BIND($other);
        $other++;
    }

    AST::Call->new
        ( identifier => string 'new',
          capture => AST::Capture->new
          ( invocant => FETCH(lookup('AdhocSignature')),
            positional => [],
            named =>
            [ string 'BIND' => AST::Block->new
              ( regs => [qw(interpreter scope capture)],
                stmts => trailing_return(\@stmts))]));
}
sub emit_m0ld {
    my $m = shift;
    if ($Mildew::adhoc_sig) {
        return $m->emit_m0ld_ahsig(@_);
    }
    my $sig = FETCH(call new => lookupf('Signature'));
    let $sig, sub {
        my $sig = shift;
        my $stmts = [map ({call register => $_->emit_m0ld,[$sig]} @{$m->{parameter}}),$sig];
        AST::Seq->new(stmts => $stmts);
    };
}
1;
