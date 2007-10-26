use v6-alpha;
class Gather is Array {
    has $.code;
    has $.buf;
    has $.finished;
    method perl {
        '( gather ' ~ $.code.perl ~ ' )' 
    };
    method Str {
        (self.eager).Str;
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
        my @res = [];
        my $arity = (&code.signature).arity;
        my $v = 0;
        while $v <= $obj.elems {
        # while !$obj.finished { 
            my @param;
            while @param.elems < $arity {
                @param.push( $obj[ $v ] );
                $v = $v + 1;
            }
            @res.push( code( |@param ) );
        };
        @res;
    };
}
