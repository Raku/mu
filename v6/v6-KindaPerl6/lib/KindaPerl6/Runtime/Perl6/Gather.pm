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
    method int  { 
        # TODO
    };
    method hash {
        # TODO
    };
    method array {
        # TODO
    };
}
