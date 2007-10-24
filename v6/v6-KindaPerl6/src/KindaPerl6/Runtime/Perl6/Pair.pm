use v6-alpha;
class Pair is Value {
    has $.key;
    has $.value;
    method perl {
        '( ' ~ $.key.perl ~ ' => ' ~ $.value.perl ~ ' )' 
    };
    method Str {
        $.key ~ "\t" ~ $.value 
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    method Int  { $.value.Int };
    #method hash {
    #    { $.key => $.value, }
    #};
    method array {
        [ $.key, $.value ]
    };
    method keys {
        [ $.key ]
    };
    method values {
        [ $.value ]
    };
    method LOOKUP {
           ( @_[0] eq $.key )   # XXX use === instead
        ?? $.value
        !! undef;
    };
}
