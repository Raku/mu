use v6-alpha;
class Gather is Array {
    has $.code;
    method perl {
        '( gather ' ~ $.code.perl ~ ' )' 
    };
    method str {
        # TODO
        '[ ... ]';   
    };
    method true {
        # TODO
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
