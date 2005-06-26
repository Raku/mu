module Perl::Compiler::PIL::Util;

class Pad {
    has Pad $.parent;
    has Str @.names;
}

class Pos {
    has Int $.line;
    has Int $.column;
}

class PosRange {
    has Str $.file;
    has Pos $.begin;
    has Pos $.end;

    has $.match;
}


role Type { }

class ConcreteType
    does Type {
    has Str $.name;
}

class TypeConjunction
    does Type {
    has Type $.left;
    has Type $.right;
}

class TypeDisjunction
    does Type {
    has Type $.left;
    has Type $.right;
}


role Context { }

class VoidContext
    does Context 
{ }

class ItemContext
    does Context {
    has Type $.type;
}

class SlurpyContext
    does Context {
    has Type @.types;
    has Bit $.unbounded;   # the last type represents a *@array of 
                           # that type if true
}


class Signature {
    has Parameter @.params;
}

class Parameter {
    has Bit $.is_invocant;
    has Bit $.is_optional;
    has Bit $.is_named;
    has Bit $.is_ref;
    has Bit $.is_rw;
    has Bit $.is_delayed;
    
    has Str $.name;
    has Context $.context;
    has Perl::Compiler::PIL::PIL $.default;
}

# vim: ft=perl6 :
