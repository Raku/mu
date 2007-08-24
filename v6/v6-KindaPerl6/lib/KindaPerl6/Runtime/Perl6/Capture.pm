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
              '\\( ' 
            ~ ( $invocant.defined 
                ?? $.invocant.perl ~ ': ' 
                !! ''
              )
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
}

