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
    method INDEX ($i) {
        my $obj = self;
        while !$obj.finished { 
                if $i < ($obj.buf).elems {
                    return ($obj.buf)[$i];
                };
                $obj._more; 
        };
        return ($obj.buf)[$i];
    };
    method map (&code) {
        my $obj = self;
        gather {
            my $i = 0;
            while !$obj.finished { 
                    take code( $obj[$i] );
                    $i = $i + 1;
            };
            while $i < ($obj.buf).elems { 
                    take code( $obj[$i] );
                    $i = $i + 1;
            };
        }; 
    };
}
