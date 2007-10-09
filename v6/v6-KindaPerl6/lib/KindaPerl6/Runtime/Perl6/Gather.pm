use v6-alpha;
class Gather is Array {
    has $.code;
    has $.buf;
    has $.finished;
    method perl {
        '( gather ' ~ $.code.perl ~ ' )' 
    };
    method str {
        # TODO
        '[ ... ]';   
    };
    method true {
        self._more;
        return $.buf.true; 
    };
    method eager  { 
        while !$.finished { self._more };
        self.buf;
    };
    method elems  { 
        (self.eager).elems;
    };
    method hash {
        # TODO
    };
    method array {
        # TODO
    };
}
