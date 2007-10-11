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
    has @.array;
    has $.hash;
    has $.return;  # ???

    method arity {
        # ??? how about optionals

        if !( defined( self.array ) ) {
            self.array = [ ];   # XXX accessor init bug
        };

        if !( defined( self.hash ) ) {
            self.hash = { };   # XXX accessor init bug
        };

        @.array.elems + $.hash.elems;
    };
    method perl {
        my $v;   # XXX kp6 ast processor bug
        my $s = ':( ';
        
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
}

