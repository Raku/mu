
=pod

Here are some examples of emitters which use a :parsetree of the rx
grammar to generate rx matcher engines.

=cut

# Given..

class bos is Match::Subpattern {};

# For PCRE... 

class PCRETreeWalk {
    # up() takes a contribution to the regexp and repacker_code.
    method up($regex, ?$repacker_code="") {[$regex,$repacker_code]}
}

multi sub emit(PCRETreeWalk $d, bos $node) {
    $d.ret($d.up('\A'));
}

# For RxOnP6...

# hypothetically using  temp $current_pos;
class RxOnP6TreeWalk {
    method pos() {'$current_pos'}
    method assert($expr) {"($expr) or fail"}
}

multi sub emit(RxOnP6TreeWalk $d, bos $node) {
    $d.ret($d.assert($d.pos()~" == 0"));
}

# For PGE...

multi sub emit(PGETreeWalk $d, bos $node) {
    $d.ret($d.term('
    $P0 = find_global "PGE::Exp", "new"
    exp = $P0("PGE::Exp::Anchor")
    exp["token"] = "^"
    '));
}
# Of course, it would really just be $b.anchor("^"),
# but that would be boring.

# Regards bootstrapping...

# Until you have a running rx, you can fake the :parsetrees used to
# drive your emitter.  You just need enough to make your emitter
# happy, so you can often skip nodes, simplifying the tree.

# if rule pattern { <term>+ } and rule rep { <term> (\+|\*) }
my $rule_pattern =
    n0(<rep>,
      [ n0(<term>, [n2(<subrule>,'name',"term")]),
	n1(<>,'+') ]);

# with helpers sortof like this:
sub n0($type,$a){
    my $m = Match::SubruleX::($type).new();
    $m.as_array = $a;
    return $m;
}
