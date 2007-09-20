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
        if (self.result) {
            return self.result;
        }
           self.bool
        ?? substr( self.match_str, self.from, self.to - self.from )
        !! undef;
    };

    method true {
        return $.bool.true;
    };

    method set_from {
        $.from = $_[0];
    };

    method set_to {
        $.to = $_[0];
    };

    method set_bool {
        $.bool = $_[0];
    };

    method set_match_str {
        $.match_str = $_[0];
    };
    method LOOKUP($key) {
        if (!(defined($.hash))) {
            $.hash = {};
        };
        return $.hash{$key};
    };
}

