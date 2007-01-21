use v6-alpha;
use Set;

sub closure($nfa, $tran, $states) {
    my @q = $states.members;
    
    # Why don't I just make @ret a set here, instead of grepping on
    # it as if it were?  Well, because if I do, then apparently
    # it's not true that 2 == 2 anymore.  Yes, very strange.  Try it.
    my @ret;

    @ret.push(@q) if $tran eq '';

    while (@q) {
        my $state = @q.shift;
        for @($nfa{$state}) {
            if .key eq $tran {
                my $value = .value;
                unless (@ret.grep:{ $value eq $_ }) {
                    @q.push($value);
                    @ret.push($value);
                }
            }
        }
    }

    return set(@ret);
}

sub transitions($nfa, $states) {
    my $ret = set();
    for ($states.members) {
        my $list = $nfa{$_};
        $ret.insert($list.map:{.key});
    }

    return $ret;
}

sub set2str($set) {
    my @elem = $set.members.sort;
    
    return @elem.join(';');
}

sub nfa2dfa($nfa, $start) {
    my $inistate = closure($nfa, '', set($start));
    my @q = ($inistate);
    my $dfa = {};
    my $seen = set();
    while (@q) {
        my $state = @q.shift;
        my $strstate = set2str($state);
        next if $seen.includes($strstate);
        $seen.insert($strstate);
        for transitions($nfa, $state).members -> $tran {
            next if $tran eq '';
            my $tranclosure = closure($nfa, $tran, $state);
            my $newstate = closure($nfa, '', $tranclosure);
            $dfa{set2str($state)}{$tran} = set2str($newstate);
            @q.push($newstate) unless $seen.includes(set2str($newstate));
        }
    }
    
    return ($dfa, set2str($inistate));
}

# nfa for /[foo+]? ba[r|z]/
my $nfa = {
    0 => [ '' => 1, '' => 5 ],
    1 => [ 'f' => 2 ],
    2 => [ 'o' => 3 ],
    3 => [ 'o' => 4 ],
    4 => [ 'o' => 4, '' => 5 ],
    5 => [ 'b' => 6 ],
    6 => [ 'a' => 7 ],
    7 => [ 'r' => 'X', 'z' => 'X' ],
    X => [],
};

my ($dfa, $start) = nfa2dfa($nfa, 0);
say "START: $start";
for $dfa.kv -> $s, $t {
    printf("%-10s : %s\n", $s, $t.perl);
}

# vim: ft=perl6 :
