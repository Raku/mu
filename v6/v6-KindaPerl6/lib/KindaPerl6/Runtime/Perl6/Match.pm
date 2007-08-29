use v6-alpha;

class Match {
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;
    has $.array;
    has $.hash;

    method str {
           self.bool 
        ?? substr( self.match_str, self.from, self.to - self.from )
        !! undef;
    };

    method true {
        return $.bool
    };

}

