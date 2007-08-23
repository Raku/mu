use v6-alpha;

class Signature::Item {
    has $.sigil;
    has $.twigil;
    has $.name;
    
    method perl {
        $.sigil ~ $.twigil ~ $.name
    }
}

class Signature is Value {
    has $.invocant;
    has $.array;
    has $.hash;
    has $.return;  # ???

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

