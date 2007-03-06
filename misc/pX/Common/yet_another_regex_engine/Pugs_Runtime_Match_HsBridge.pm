package Pugs::Runtime::Match::HsBridge;
BEGIN{unshift(@INC,"misc/pX/Common/yet_another_regex_engine")}
@ISA=qw(Pugs::Runtime::Match::HsBridgeBase);
use strict;
use warnings;
use Pugs_Runtime_Match_HsBridgeBase;
use Regexp_ModuleA;
BEGIN{Regexp::ModuleA::Api::PreludeA->import}
sub compile {
    my($o,$re,$ops)=@_;
    my $pkg = $ops->{grammar}; delete $ops->{grammar};
    my $mods = join("",map{":${_}<$ops->{$_}>"} keys %$ops);
    Regexp::ModuleA::Api::RegexApi0->create('regex',undef,$re,mods=>$mods);
}
sub reinstall {
    my($o,$name,$re,$ops)=@_;
    my $pkg = $ops->{grammar}; delete $ops->{grammar};
    my $mods = join("",map{":${_}<$ops->{$_}>"} keys %$ops);
    Regexp::ModuleA::Api::RegexApi0->create('regex',$name,$re,mods=>$mods);
}
sub match_dump_hs {
    my($cls,$m)=@_;
    return "PGE_Fail" if !$m;
    dump_hs($m);
}
sub dump_hs {
    my($x)=@_;
    my $ref = ref $x;
    if (!$ref) {
        my $str = shift;
        $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
        return "PGE_String \"$str\"";
    }
    elsif ($ref eq 'ARRAY') {
        return "PGE_Array [" . join(', ', map { dump_hs($_) } @$x) . "]"
    }
    else {
	join(' ', 'PGE_Match', $x->from, $x->to,
	     ('['.join(', ', map { dump_hs($_) } @$x) .']'),
	     ('['.join(', ', map {
		 my $str = $_;
		 my $dump = dump_hs($x->{$_});
		 $str =~ s/([^ \!\#\$\%\&\x28-\x5B\x5D-\x7E])/'\\'.ord($1)/eg;
		 qq[("$str", $dump)];
	     } sort(keys %$x) ).']')
	     );
    }
}
