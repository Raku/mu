use v6-alpha;
class Gather is Array {
    has $.code;
    has $.buf;
    has $.finished;
    method perl {
        '( gather ' ~ $.code.perl ~ ' )' 
    };
    method str {
        (self.eager).str;
    };
    method true {
        self._more;
        return $.buf.true; 
    };
    method eager  { 
        while !$.finished { self._more };
        self.buf;
    };
    method lazy {
        self
    };
    method elems  { 
        (self.eager).elems;
    };
    method hash {
        (self.eager).hash;
    };
    method array {
        self
    };
    method map (&code) {
        gather {
            my $i = 0;
            while !$.finished { 
                say "a $i code ",&code;
                self._more; 
                if !$.finished {
                    my $r = (self)[$i];
                    say "got $r";
                    $r = code( $r );
                    say "got() $r";
                    take $r; 
                    $i = $i + 1;
                };
            };
            while $i <= (self.buf).elems { 
                say "b $i";
                take code( (self)[$i] ); 
                $i = $i + 1;
            };
        };
    };
}
