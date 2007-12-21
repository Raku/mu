module Unicode;

constant Int $unicode_max = 0x10ffff;

class Utable {
    # An efficient data structure for unicode property data
    # The basic data structure is an array of Range objects
    # e.g. <alpha> would use ( 0x41..0x5a ; 0x61..0x7a ; ... )
    # lookup is O(log n), where n is the number of ranges, not total codepoints
    # insertion is O(n) in general, but appending a non-contuguous range is O(1)
    has Range @@.table;

    multi submethod BUILD(Range @@r) {
        @@.table = @@r;
        $.preen;
    }
    multi submethod BUILD(Str $str) {
        # parse the output of $.print
        for $str.split(';') {
            if mm/^ ( <xdigit>+ ) $/ {
                $.add(hex $0, :!preen);
            } elsif mm/^ ( <xdigit>+ ) '..' ( <xdigit>+ ) $/ {
                $.add(hex($0) .. hex($1), :!preen);
            } else {
                die "Utable::BUILD: can't parse '$_'";
            }
        }
        $.preen;
    }

    method print(-->) {
        # not needed if @@.table.perl DTRT...
        for @@.table -> $r {
            print ';' if my Int $n++;
            if $r.min == $r.max {
                printf '%x', $r.min;
            } else {
                printf '%x..%x', $r.min, $r.max;
            }
        }
    }

    method say(-->) { $.print; say; }

    method contains(Int $x --> Bool) {
        return False if !+@@.table;
        return False if $x < @@.table[0].min;
        return False if $x > @@.table[*-1].max;
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            my Int $mid = ($max + $min) / 2;
            return True if $x ~~ @@.table[$mid];
            if $x < @@.table[$mid].min  {
                $max = $mid - 1;
            } else {
                $min = $mid + 1;
            }
        }
        return False;
    }

    method inverse(--> Utable) {
        my Utable $u.=new;
        if !+@@.table {
            $u.add(0 .. $unicode_max);
            return $u;
        }
        $u.add(0 ..^ @@.table[0].min);
        # $i < @@.table.elems-1 is intended
        loop my Int $i = 0; $i < @@.table.elems-1; $i++ {
            $u.add(@@.table[$i].max ^..^ @@.table[$i+1].min, :!preen);
        }
        $u.add(@@.table[*-1].max ^.. $unicode_max);
        return $u;
    }

    multi method add(Int $x, Bool :$preen = True -->) {
        return if $.contains($x);
        my Range $r = $x .. $x;
        if !+@@.table {
            @@.table[0] = $r;
            return;
        }
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            if $x < @@.table[$min].min {
                @@.table.=splice: $min, 0, $r;
                $.preen if $preen;
                return;
            }
            if $x > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $r;
                $.preen if $preen;
                return;
            }
            my Int $mid = ($max + $min) / 2;
            if $x+1 == @@.table[$mid].min {
                @@.table[$mid].min = $x;
                $.preen if $preen;
                return;
            } elsif $x-1 == @@.table[$mid].max {
                @@.table[$mid].max = $x;
                $.preen if $preen;
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

    multi method add(Range $r, Bool :$preen = True -->) {
        if !+@@.table {
            @@.table[0] = $r;
            $.preen if $preen;
            return;
        }
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            if $r.max < @@.table[$min].min {
                @@.table.=splice: $min, 0, $r;
                $.preen if $preen;
                return;
            }
            if $r.min > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $r;
                $.preen if $preen;
                return;
            }
            my Int $mid = ($max + $min) / 2;
            my Range $m := @@.table[$mid];
            if ( $r.max >= $m.min and $r.min <= $m.min ) or ( $r.max >= $m.max and $r.min <= $m.max ) {
                # $r and $m overlap
                $m = min($r.min, $m.min) .. max($r.max, $m.max);
                $.preen if $preen;
                return;
            } else {
                if $r.max < @@.table[$mid].min {
                    $max = $mid - 1;
                } else {
                    $min = $mid + 1;
                }
            }
        }
        die "Utable::add got lost somehow";
    }

    method preen(-->) {
        # delete null ranges, fix up range overlaps and contiguities
        # $i < @@.table.elems-1 is intended
        loop my Int $i = 0; $i < @@.table.elems-1; $i++ {
            @@.table[$i].delete if @@.table[$i].max < @@.table[$i].min;
            if @@.table[$i].max >= @@.table[$i+1].min {
                @@.table.=splice: $i, 2, @@.table[$i].min .. @@.table[$i+1].max;
            }
        }
    }
}
