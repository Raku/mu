use v6-alpha;
class Range is Value {
    has $.start;
    has $.end;
    method perl {
        '( ' ~ $.start.perl ~ '..' ~ $.end.perl ~ ' )' 
    };
    method Str {
        $.start ~ ".." ~ $.end 
    };
    #method true { true };
    #method Int  { $.start.Int };
    #method hash {
    #    { $.start => $.end, }
    #};
    #method array {
    #    [ $.start, $.end ]
    #};
}
