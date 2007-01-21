use v6-alpha;
use Set;

sub epsilon_closure($nfa, $states) {
    my @q = $states.members;
    
    # Why don't I just make @ret a set here, instead of grepping on
    # it as if it were?  Well, because if I do, then apparently
    # it's not true that 2 == 2 anymore.  Yes, very strange.  Try it.
    my @ret;

    while (@q) {
        my $state = @q.shift;
        unless (@ret.grep:{ $state eq $_ }) {
            @ret.push($state);
            for @($nfa{$state}) {
                if .key eq '' {
                    @q.push(.value);
                }
            }
        }
    }

    return set(@ret);
}

sub scan($nfa, $states, $tran)  {
    my $ret = set();
    for ($states.members) -> $state {
        for @($nfa{$state}) {
            if .key eq $tran {
                $ret.insert(.value);
            }
        }
    }
    return $ret;
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
    my $inistate = epsilon_closure($nfa, set($start));
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
            my $scan = scan($nfa, $state, $tran);
            my $newstate = epsilon_closure($nfa, $scan);
            $dfa{set2str($state)}{$tran} = set2str($newstate);
            @q.push($newstate) unless $seen.includes(set2str($newstate));
        }
    }
    
    return ($dfa, set2str($inistate));
}

# nfa for /foo*[ba|oba]*[r|z]/
my $nfa = {
    0  => [ 'f' => 1 ],
    1  => [ 'o' => 2 ],
    2  => [ 'o' => 2, '' => 3 ],
    3  => [ ''  => 4, '' => 7 ],
    4  => [ 'b' => 5 ],
    5  => [ 'a' => 6 ],
    6  => [ ''  => 3, '' => 11 ],
    7  => [ 'o' => 8 ],
    8  => [ 'b' => 9 ],
    9  => [ 'a' => 10 ],
    10 => [ ''  => 3, '' => 11 ],
    11 => [ 'r'  => 'X', 'z' => 'X' ],
};

my ($dfa, $start) = nfa2dfa($nfa, 0);
say "START: $start";
for $dfa.kv -> $s, $t {
    printf("%-13s : %s\n", $s, $t.perl);
}

# vim: ft=perl6 :
