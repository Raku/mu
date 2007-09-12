use v6-alpha;
class Capture is Value {
    has $.invocant;
    has $.array;
    has $.hash;

    method arity {
        # ??? how about optionals
        @.array.elems + $.hash.elems;
    };
    method perl {
        my $v;   # XXX kp6 ast processor bug
        my $s = '\\( ';
        
        if $.invocant.defined {
            $s = $s ~ $.invocant.perl ~ ': ';
        };
        
        for @.array -> $v { 
            $s = $s ~ $v.perl ~ ', ';
        };
        
        # TODO
        #for $.hash.pairs -> $v {
        #    $s = $s ~ $v.perl ~ ', ';
        #};
        
        return $s ~ ' )' 
    };
    method str {
        self.perl;
    };
    #method true { self.elems != 0 };
    #method int  { self.elems };
}

