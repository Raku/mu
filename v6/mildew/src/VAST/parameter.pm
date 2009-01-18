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
      } elsif ($m->{quant} eq '\\') {
        call 'STORE' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT}]),
			  [call 'positional' => reg '$capture',[integer($count) ]];;
      } else {
        call 'BIND' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{identifier}[0]{TEXT}]),
			  [call 'positional' => reg '$capture',[integer($count) ]];
      }
    } elsif ($m->{named_param}) {
        my $ident = $m->{named_param}{param_var}{identifier}[0]{TEXT};
        my $sigil = $m->{named_param}{param_var}{sigil}{sym};
        call 'BIND' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $sigil.$ident ]),
			  [call 'named' => reg '$capture',[string $ident ]];
    } else {
        XXX;
    }
}


1;
