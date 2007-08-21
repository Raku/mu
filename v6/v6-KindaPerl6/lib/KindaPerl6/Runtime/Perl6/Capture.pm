use v6-alpha;
class Capture is Value {
    has $.invocant;
    has $.array;
    has $.hash;

    # TODO ...
    method perl {
              '\\( ' 
            ~ $.invocant.perl ~ ': ' 
            ~ ( $.array.elems 
                ?? $.array.perl ~ ', ' 
                !! '' 
              )
            ~ ( $.hash.elems
                ?? $.hash.perl
                !! ''
              )
            ~ ' )' 
    };
    method str {
        self.perl;
    };
    #method true { self.elems != 0 };
    #method int  { self.elems };

    # TODO ...
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

