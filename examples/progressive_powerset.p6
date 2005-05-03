use v6;

sub iter_powerset ( *@factor ) returns Ref {
    my $end = @factor.elems - 1;
    my @subset = (undef) xx $end;
    my ($pos, $mode) = (-1, 1);
    my $return = { list @factor[ grep { defined $_ } @subset ] };
    my %dispatch = (
        1 => {
            ++$pos;
            @subset[ $pos ] = $pos;
            ++$mode if $pos == $end;
            $return();
        },
        2 => {
            @subset[ $pos - 1 ] = undef;
            ++$mode;
            $return();
        },
        3 => {
            @subset[ $pos-- ] = undef;
            while ( $pos >= 0 ) {
                last if defined @subset[ $pos ];
                --$pos;
            }
            @subset[ $pos++ ] = undef;
            return if !$pos;
            @subset[ $pos ] = $pos;
            $mode = 1;
            $return();
        },
    );
    return { %dispatch{ $mode }() };
}

my $next = iter_powerset( 1..5 );
my @combo;
while ( @combo = $next() ) {
    @combo.say;
}
