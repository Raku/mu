module Dumper-0001;
use v6-alpha;

sub dump is export(:ALL) {
    for (@_) {
        my $what = _dumper(0, '$DUMP6', $_);
        if ~$what {
            say "\$DUMP6 = $what $_";
        }
    }   
}

sub _dumper ($s, $x, $y) {
    my $what = $y.WHAT;

    if Array ~~ $what {
        say " " x $s, $x, " = [";
        _dump_array($s, $y);
        say " " x $s, "]";
    } elsif Hash ~~ $what {
        say " " x $s, $x, " = \{";
        _dump_hash($s, $y);
        say " " x $s, "}";
    } else {
        return $what;
    }

    return 0;
}

sub _dump_hash ($s is copy, %h) {
    $s += 2;
    for %h.kv -> $k, $v {
        my $what = _dumper($s, $k, $v);
        if ~$what {
            say " " x $s, "$k = $what $v";
        }
    }
}

sub _dump_array ($s is copy, @a) {
    $s += 2;
    my $i = 0;
    for @a -> $v {
        my $what = _dumper($s, $i, $v);
        if ~$what {
            say " " x $s, "$i = $what $v";
        }
        $i++;
    }
}

1;
