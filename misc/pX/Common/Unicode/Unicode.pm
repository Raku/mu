module Unicode;

class Utable {
    # An efficient data structure for unicode property data
    # The basic data structure is an array of Range objects
    # e.g. <alpha> would use ( 0x41..0x5a ; 0x61..0x7a ; ... )
    # lookup and insertion are done via binary search
    has Range @@.table;

    multi submethod BUILD(Range @@r) {
        @@.table = @@r;
    }
    multi submethod BUILD(Str $str) {
        # parse the output of $.print
        my @s = split $str, ' ; ';
        for @s {
            if mm/^ ( <xdigit>+ ) $/ {
                $.add(hex $0);
            } elsif mm/^ ( <xdigit>+ ) '-' ( <xdigit>+ ) $/ {
                $.addrange(hex($0) .. hex($1));
            } else {
                die "Utable::BUILD: can't parse '$_'";
            }
        }
    }

    method print(-->) {
        # not needed if @@.table.perl DTRT...
        for @@.table -> $r {
            print ' ; ' if my $n++;
            if $r.min == $r.max {
                printf '%x', $r.min;
            } else {
                printf '%x-%x', $r.min, $r.max;
            }
        }
    }

    method say(-->) { $.print; say; }

    method contains(Int $x --> Bool) {
        return False if !+@@.table;
        return False if $x < @@.table[0].min;
        return False if $x > @@.table[*-1].max;
        return $.contains($x, 0, @@.table.elems-1);
    }

    method contains(Int $x, Int $min, Int $max --> Bool) {
        my $mid = ($max + $min) / 2;
        return True if $x ~~ @@.table[$mid];
        return False if $min == $max;
        if $x < @@.table[$mid].min  {
            return $.contains($x, $min, $mid-1);
        } else {
            return $.contains($x, $mid+1, $max);
        }
    }

    method add(Int $x -->) {
        return if $.contains($x);
        if !+@@.table {
            @@.table[0] = $x .. $x;
            return;
        }
        my $min = 0;
        my $max = @@.table.elems-1;
        while $min <= $max {
            if $x < @@.table[$min].min {
                @@.table.=splice: $min, 0, $x .. $x;
                $.connect;
                return;
            }
            if $x > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $x .. $x;
                $.connect;
                return;
            }
            my $mid = ($max + $min) / 2;
            if $x+1 == @@.table[$mid].min {
                @@.table[$mid].min = $x;
                $.connect;
                return;
            } elsif $x-1 == @@.table[$mid].max {
                @@.table[$mid].max = $x;
                $.connect;
                return;
            } else {
                if $x < @@.table[$mid].min {
                    $max = $mid - 1;
                } else {
                    $min = $mid + 1;
                }
            }
        }
        die "Utable::add got lost somehow";
    }

    method addrange(Range $r -->) {
        if !+@@.table {
            @@.table[0] = $r;
            return;
        }
        my $min = 0;
        my $max = @@.table.elems-1;
        while $min <= $max {
            if $r.max < @@.table[$min].min {
                @@.table.=splice: $min, 0, $r;
                $.connect;
                return;
            }
            if $r.min > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $r;
                $.connect;
                return;
            }
            my $mid = ($max + $min) / 2;
            my $m := @@.table[$mid];
            if ( $r.max >= $m.min and $r.min <= $m.min ) or ( $r.max >= $m.max and $r.min <= $m.max ) {
                # $r and $m overlap
                $m = min($r.min, $m.min) .. max($r.max, $m.max);
                $.connect;
                return;
            } else {
                if $r.max < @@.table[$mid].min {
                    $max = $mid - 1;
                } else {
                    $min = $mid + 1;
                }
            }
        }
        die "Utable::addrange got lost somehow";
    }

    method connect(-->) {
        # fix up range overlaps
        # $i < @@.table.elems-1 is intended
        loop my $i = 0; $i < @@.table.elems-1; $i++ {
            if @@.table[$i].max >= @@.table[$i+1].min {
                @@.table.=splice: $i, 2, @@.table[$i].min .. @@.table[$i+1].max;
            }
        }
    }
}
