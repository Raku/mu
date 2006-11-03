use v6-alpha;

class CompUnit {
    has $.class;
    has $.attributes;
    has $.methods;
    has $.body;
}

class Val::Int {
    has $.int;
    method emit {
        $.int
    }
}

class Val::Bit {
    has $.bit;
    method emit {
        "EMIT: " ~ $.bit
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        $.sigil ~ $.twigil ~ $.name
    }
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        $.parameters.emit ~ ' = ' ~ $.arguments.emit
    }
}

