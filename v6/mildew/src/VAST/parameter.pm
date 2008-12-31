package VAST::parameter;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld_ahsig_BIND_invocant {
    my ($m) = @_;
    if ($m->{param_var}) {
#        call STORE => (call 'postcircumfix:{ }' => reg '$scope',[string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT} ]),[call FETCH => (call invocant => reg '$capture')];
        call BIND => (call 'postcircumfix:{ }' => reg '$scope',[string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT} ]),[call invocant => reg '$capture'];
    } else {
        XXX;
    }
}

sub emit_m0ld_ahsig_BIND {
    my ($m, $count) = @_;
    if ($m->{param_var}) {
      if ($m->{quant} eq '|') {
        call 'STORE' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT}]),
			  [ reg '$capture' ];
      } else {
        call 'BIND' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT}]),
			  [call 'positional' => reg '$capture',[integer($count) ]];
      }
    } else {
        XXX;
    }
}


1;
