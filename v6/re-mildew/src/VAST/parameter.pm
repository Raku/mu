package VAST::parameter;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld_ahsig_BIND {
    my ($m, $count) = @_;
    if ($m->{param_var}) {
      if ($m->{quant} eq '|') {
        call 'STORE' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{name}[0]{TEXT}]),
			  [ reg '$capture' ];
      } elsif ($m->{quant} eq '\\') {
        call 'STORE' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{name}[0]{TEXT}]),
			  [call 'positional' => reg '$capture',[integer($count) ]];;
      } else {
        call 'BIND' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{name}[0]{TEXT}]),
			  [call 'positional' => reg '$capture',[integer($count) ]];
      }
    } elsif ($m->{named_param}) {
        my $ident = $m->{named_param}{param_var}{name}[0]{TEXT};
        my $sigil = $m->{named_param}{param_var}{sigil}{sym};
        call 'BIND' => (call 'postcircumfix:{ }' => reg '$scope',
			[ string $sigil.$ident ]),
			  [call 'named' => reg '$capture',[string $ident ]];
    } else {
        XXX;
    }
}
sub emit_m0ld {
    my $m = shift;
    my $type;
    my $trait = $m->{trait}[0]{trait_auxiliary}{longname}{name}{identifier}{TEXT} || 'readonly';
    if ($trait eq 'ref') {
        $type = 'RefParam';
    } elsif ($trait eq 'readonly') {
        $type = 'ReadonlyParam';
    } else {
        die "unknow type of param $trait";
    }
    my $param = FETCH(call new => lookupf($type));
    let $param, sub {
        my $param = shift;
        AST::Seq->new(stmts => [
            call(STORE => (call name => $param),[ string $m->{param_var}{sigil}{sym}.$m->{param_var}{name}[0]{TEXT}]),
            $param]
        );
    }
}


1;
