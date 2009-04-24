package VAST::signature;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld_ahsig {
    my $m = shift;

    my $other = 0;
    my @stmts;

    # TODO invocant
    #push @stmts, call BIND => (call 'postcircumfix:{ }' => reg '$scope',[string '$¿self']),[call invocant => reg '$capture'];

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
1;
