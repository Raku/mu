use v6-alpha;
class Range is Value {
    has $.start;
    has $.end;
    method perl {
        '( ' ~ $.start.perl ~ '..' ~ $.end.perl ~ ' )' 
    };
    method str {
        $.start ~ ".." ~ $.end 
    };
    #method true { true };
    #method int  { $.start.int };
    #method hash {
    #    { $.start => $.end, }
    #};
    #method array {
    #    [ $.start, $.end ]
    #};
}
