use v6-alpha;
class Pair {
    has $.key;
    has $.value;
    method perl {
        '( ' ~ $.key ~ ' => ' ~ $.value ~ ' )' 
    };
    method str {
        $.key ~ '   ' ~ $.value 
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    method int  { $.value.int };
}
