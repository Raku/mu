=pod

MatchX is a temporary alternative to Match.

Match is currently(r5000) a primitive, slightly off-spec, and immutable.
So MatchX is easier to use when you are creating or modifying matches.
Such as when you are writing a regexp engine.

=cut


#http://www.nntp.perl.org/group/perl.perl6.language/20985

class MatchX {
    #does MatchRole;
    has $.as_b;
    has $.as_i;
    has $.as_s;
    has $.as_a;
    has $.as_h;
    has $.from;
    has $.to;

    submethod BUILD() {
	./set(1,1,"",[],hash);
    }
    multi method set($b,$i,$s,$a,$h,?$f,?$t) {
	$.as_b = $b;
	$.as_i = $i;
	$.as_s = $s;
	$.as_a = $a;
	$.as_h = $h;
	$.from = $f;
	$.to = $t;
	$?SELF;
    }
    multi method set_as_failed() {
	./set(0,0,"",[],hash);
	$?SELF;
    }
    multi method set_from_match($m) {
	if !$m {
	    ./set_as_failed;
	} else {
	    $.as_b = $m ?? bool::true :: bool::false;#?$m;
	    $.as_i = 0+$m;#+$m; #Should usually be 1.
	    $.as_s = ""~$m;#~$m;
	    $.as_a = [@$m];
	    $.as_h = hash %$m;
	    $.from = $m.from;
	    $.to = $m.to;
	}
	$?SELF;
    }

    multi method prefix:<?> {$.as_b}
    multi method prefix:<+> {$.as_i}
    multi method prefix:<~> {$.as_s}
    multi method prefix:<@> {$.as_a}
    multi method prefix:<%> {$.as_h}

    multi method to_b() {$.as_b}
    multi method to_i() {$.as_i}
    multi method to_s() {$.as_s}
    multi method to_a() {$.as_a}
    multi method to_h() {$.as_h}

    multi method from() {$.from}
    multi method to()   {$.to}
}
class MatchX::Submatch is MatchX {
}
# Match::Submatch is called Match::Subrule in Damian's note.
# I've found that just too confusing, given that "subrule"
# also names such a related but different concept. -putter


# multi method describe() returns String
# <MatchX::Submatch::Escape,1,1,'x',[
#   <Match::Submatch,1,1,'x',[],{}>
#  ],{}>

multi method MatchX::describe() returns String {
    my $cls = .ref; $cls ~~ s:perl5/\A:://;
    my $b = $.as_b ?? 1 :: 0;
    my @ae = $.as_a;
    my $a = join(",\n",map {$_.describe}, @ae);
    if $a ne "" {
	$a = MatchX::indent(2,$a);
	$a = "\n$a\n "; }
    my $h = join(",\n", map -> $k,$v {
	"$k =>" ~ MatchX::indent(1,$v.describe);
	}, $.as_h.kv);
    if $h ne "" {
	$h = MatchX::indent(2,$h);
	$h = "\n$h\n "; }
    "<$cls,$.as_b,$.as_i,'$.as_s',[$a],\{$h}>"
}
multi method Match::describe() returns String {
    my $m = $?SELF;
    my $cls = $m.ref; $cls ~~ s:perl5/\A:://;
    my $b = $m ?? 1 :: 0;
    my $i = 0+$m;
    my $s = ""~$m;
    my $a = join(",\n",map {$_.describe},@$m);
    if $a ne "" {
	$a = MatchX::indent(2,$a);
	$a = "\n$a\n "; }
    my $h = join(",\n", map -> $k,$v {
	"$k =>" ~ MatchX::indent(1,$v.describe);
	}, %$m);
    if $h ne "" {
	$h = MatchX::indent(2,$h);
	$h = "\n$h\n "; }
    "<$cls,$b,$i,'$s',[$a],\{$h}>"
}
sub MatchX::indent($n,$str) returns String {
    #$a ~~ s:perl5:g/^/  /;#pugsbug-nonterminates
    my $s = $str;
    my $sp = " " x $n;
    $s ~~ s:perl5:g/\n/$nl$sp/;
    $s = $sp~$s;
    return $s;
}

