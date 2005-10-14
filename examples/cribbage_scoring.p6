use v6;

# Brute force proof that every cribbage hand with a 5 is >= 2 points
# See http://perlmonks.org/index.pl?node_id=458728 for details

# The following code will not work yet as of revision 4167
# There are two bugs and two unimplemented features
# Bug 1     - t/pugsbugs/return_with_trailing_stuff.t
# Bug 2     - t/pugsbugs/postincrement_in_subscripts.t
# Feature 1 - t/operators/hyper.t (hyper dereferencing)
# Feature 2 - t/statements/last.t (last <label>)

my $next = combo(5, new_deck());
while $combo == 1 {
    # Skip all hands that do not contain a 5
#    next if none( @combo>>.<val> ) == 5;

    # Skip all hands that have a score of at least 2
#    next if score( @combo ) > 1;

    # Print out the rest
#    say ~@combo>>.<suit>;
}

sub score ( @hand ) returns Int {
    my $score = 0;

    # [234] of a kind
    my %ordval;
    for @hand>>.<num> { %ordval{$_}++ };
    for %ordval.values { $score += $_ * $_ - 1 }

    # Flush
    $score += ([eq] @hand[0..3]>>.<suit>)
        ?? ([eq] @hand[3,4]>>.<suit>) ?? 5 !! 4
        !! 0;

    # Check for right-jack, @hand[-1] is community card
    $score++ if grep { $_<num> == 11 && $_<suit> eq @hand[-1]<suit> } @hand[0..3];

    # Count 15's
    my @vals = @hand>>.<val>;
    for 2 .. 5 {
        my $next = combo($_, @vals);
        while my @combo = $next() { $score += 2 if ([+] @combo) == 15 }
    }

    # Runs
    SPAN:
    for 5, 4, 3 -> $span {
        for sort { $^a <=> $^b } %ordval.keys -> $start {
            if all( %ordval{$start .. $start + $span} ) > 1 {
                $score += [*] %ordval{$start .. $start + $span}, $span;
                last SPAN;
            }
        }
    }
    return $score;
}

sub combo (Int $by is copy, @list is copy) returns Ref {
    my @position = 0 .. $by - 2, $by - 2;
    my @stop     = @list.elems - $by .. @list.end;
    my $done     = undef;
    return sub {
        return () if $done;
        my $cur = @position.end;
        while ++@position[ $cur ] > @stop[ $cur ] {
            @position[ --$cur ]++;
            next if @position[ $cur ] > @stop[ $cur ];
            my $new_pos = @position[ $cur ];
            @position[ $cur .. @position.end ] = $new_pos .. $new_pos + $by;
            last;
        }
        $done = 1 if @position[ 0 ] == @stop[ 0 ];
        return @list[ @position ];
    };
}

sub new_deck () returns Array {
    return map -> $num {
        map -> $suit {
            { num => $num, val => $num > 10 ?? 10 !! $num, suit => $suit }
        } <H D C S>;
    } 1..13;
}
