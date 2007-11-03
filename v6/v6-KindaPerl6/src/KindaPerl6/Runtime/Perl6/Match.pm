use v6-alpha;

class Match is Capture {
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;

    method Str {
        if ($.result) {
            return $.result.Str;
        }
           $.bool
        ?? substr( $.match_str, $.from, $.to - $.from )
        !! undef;
    };

    method scalar {
        if ($.result) {
            return $.result;
        } else {
            return self.Str()
        }
    };

    method true {
        return ($.bool).true;
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
}

