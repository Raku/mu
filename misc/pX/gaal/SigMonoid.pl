use v6;

sub sigAppend(Sig $x is copy, Sig $y is copy --> Sig) {
    # assume for simplicity that params also know their own
    # type (positional/named etc.) and mandatoriness

    if $y.posList.size > $x.posList.size -> { swap $x, $y }


    # first, go over all positionals, relegating things away to nameds
    # as needed.

    @posList = undef xx max($x.posList.size, $y.posList.size);

    for (longzip $x.posList, $y.posList).kv -> [$xp, $yp], $i -> {
        $killx = { undefine @x.posList[$i] };
        $killy = { undefine @x.posList[$i] };

        given $xp {

            when undef {
                break if $yp ~~ undef;   # prior kill
                @posList[$i] = token_from $yp :optional;
                $y.namedSet.insert($yp);
                $killy();
            }

            when { $yp ~~ undef } {
                @posList[$i] = token_from $xp :optional;
                $x.namedSet.insert($xp);
                $killx();
            }
        
            my $name;
            when { $name = $xp ~name~ $yp } {
                @posList[$i] = $xp ~unify~ $yp;
                $killx();
                $killy();
            }

            my $o;
            when { $o = otherPos($y, $i) } {
                my $op = $y.posList[$o];
                my $optional = $xp.optional || $op.optional;
                my $sigil = $xp.sigil ~~ $op.sigil ?? $xp.sigil !! '$';

                # relegate to named
                @posList[$i] = token_form :sigil<$sigil> :optional<$optional>;
                $x.namedSet.insert( $xp ~unify~ $op );

                $killx();
                undefine $y.posList[$o];
            }

            default { # pos param only in $x
                ...; # <- continue here
            }
                
            }
        }
    }
}

# vim: set ft=perl6 et ts=4 sw=4 :
