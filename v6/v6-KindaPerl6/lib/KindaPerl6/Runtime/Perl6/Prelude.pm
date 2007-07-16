#
# kp6 "Prelude"
#
# NOTE: this file must be compiled with kp6 - not mp6!!!
#
# $ perl kp6-perl5.pl < lib/KindaPerl6/Runtime/Perl6/Prelude.pm | perltidy > lib5/KindaPerl6/Runtime/Perl6/Prelude.pm
#

class Match {
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;
    has $.array;
    has $.hash;
}

class Signature {
    has $.scalar;
    has $.array;
    has $.hash;
}

class Capture {
    has $.scalar;
    has $.array;
    has $.hash;
}

# XXX "does Container" ???
class Scalar does Container {
    has $.value;
}

class Array does Container {
    has $.value;
}

class Hash does Container {
    has $.value;
}

