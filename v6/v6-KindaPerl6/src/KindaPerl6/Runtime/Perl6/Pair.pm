use v6-alpha;
class Pair is Value {
    has $.key;
    has $.value;
    method perl {
        '( ' ~ $.key.perl ~ ' => ' ~ $.value.perl ~ ' )' 
    };
    method str {
        $.key ~ "\t" ~ $.value 
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    method int  { $.value.int };
    method hash {
        { $.key => $.value, }
    };
    method array {
        [ $.key, $.value ]
    };
}
