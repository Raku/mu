use v6-alpha;
class Capture is Value {
    has $.invocant;
    has $.positional;
    has $.named;

    # TODO ...
    method perl {
        my $v;   # XXX kp6 ast processor bug
        my $s = '[ ';
        for @(self) -> $v { 
            $s = $s ~ $v.perl ~ ', ';
        };
        return $s ~ ' ]' 
    };
    method str {
        self.join( ' ' );
    };
    method true { self.elems != 0 };
    method int  { self.elems };

    my sub match_type( $spec, $thing ) {

        42

        #if $m.from == 2 {
        #    say "ok 2 - accessor";
        #}
        #else {
        #    say "not ok - got ", $m.from;
        #};

        #if $thing.HOW == $spec.HOW {
        #    return true;
        #}
        #else {
        #    return false;
        #}
    }
}

