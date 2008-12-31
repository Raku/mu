package VAST::signature;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld_ahsig {
    my $m = shift;

    my $pc = 0;
    my $other = 0;
    my @stmts;
    push @stmts, call BIND => (call 'postcircumfix:{ }' => reg '$scope',[string '$¿self']),[call invocant => reg '$capture'];
    for my $param (@{$m->{parameter}}) {
        if ($m->{param_sep}[$pc]{TEXT} && $m->{param_sep}[$pc]{TEXT} =~ /\s*:\s*/) {
            push @stmts, $param->emit_m0ld_ahsig_BIND_invocant();
        } else {
            push @stmts, $param->emit_m0ld_ahsig_BIND($other);
            $other++;
        }
        $pc++;
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

1;
