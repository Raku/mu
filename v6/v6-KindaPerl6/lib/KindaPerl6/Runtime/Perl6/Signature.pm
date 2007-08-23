use v6-alpha;
class Signature is Value {
    has $.invocant;
    has $.array;
    has $.hash;

    # TODO ...
    method perl {
              ':( ' 
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
}

