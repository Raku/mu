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
    method INDEX ($ix) {
            while !$.finished { 
                if $ix < (self.buf).elems 
                {
                    return (self.buf)[$ix];
                };
                self._more; 
            };
            return (self.buf)[$ix];
    };
    method map (&code) {
        gather {
            my $i = 0;
            while !$.finished { 
                    my $r = (self)[$i];
                    $r = code( $r );
                    take $r; 
                    $i = $i + 1;
            };
            while $i < (self.buf).elems { 
                    my $r = (self)[$i];
                    $r = code( $r );
                    take $r; 
                    $i = $i + 1;
            };
        };
    };
}
