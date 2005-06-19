#http://www.nntp.perl.org/group/perl.perl6.language/20985

class MatchX {
    #does Match;
    has $.as_b;
    has $.as_i;
    has $.as_s;
    has $.as_a;
    has $.as_h;

    multi method set($b,$i,$s,$a,$h) {
	$.as_b = $b;
	$.as_i = $i;
	$.as_s = $s;
	$.as_a = $a;
	$.as_h = $h;
    }
    multi method init() {
	./set(1,1,"",[],hash);
	$?SELF;
    }
    method init_from($match) {
	./init;
	if !$match { ./failed; }
	else {$.as_s = ~$match;}
	$?SELF;
    }
    method failed() {
	./set(0,0,"",[],hash);
	$?SELF;
    }

    #method &prefix:<?> { $.as_b }
    #method &prefix:<+> { $.as_i }
    #method &prefix:<~> { $.as_s }
    #method &prefix:<@> { $.as_a }
    #method &prefix:<%> { $.as_h }

    method to_b() {$.as_b}
    method to_i() {$.as_i}
    method to_s() {$.as_s}
    method to_a() {$.as_a}
    method to_h() {$.as_h}

    #method kv { $.as_h.kv } #???
    #method keys { $.as_h.keys } #???
    #method values { $.as_h.values } #???
    #method from {...}
    #method to {...}
}
class MatchX::Submatch is MatchX {
}

# multi method describe(MatchX $m) returns String
# <MatchX::Submatch::Escape,1,1,'x',[
#   <Match::Submatch,1,1,'x',[],{}>
#  ],{}>

# Match::Submatch is called Match::Subrule in Damian's note.
# I've found that just too confusing, given that "subrule"
# also names such a related but different concept. -putter
