use v5.10;
use MooseX::Declare;
use utf8;
class VAST::statement_control__S_unless {
    use AST::Helpers;
    method emit_m0ld {
        my $then = call 'postcircumfix:( )' => code($self->{xblock}{pblock}{blockoid}),[capturize];
        AST::If->new
            ( cond => $self->{xblock}{EXPR}->emit_m0ld,
              then => lookupf('False'),
              else => $then )
    }
}
