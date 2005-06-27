class Perl::Compiler::PIL::Util::Pad {
    has Perl::Compiler::PIL::Util::Pad $.parent;
    has Int $.max;
    has Int %.index;

    method add($name) {
        %.index{$name} = $.max++;
    }

    method lookup_pad($name) {
        if %.index.exists($name) {
            $?SELF;
        }
        else {
            $.parent ?? $.parent.lookup_pad($name) :: fail;
        }
    }
}

class Perl::Compiler::PIL::Util::Pos {
    has Int $.line;
    has Int $.column;
}

class Perl::Compiler::PIL::Util::PosRange {
    has Str $.file;
    has Perl::Compiler::PIL::Util::Pos $.begin;
    has Perl::Compiler::PIL::Util::Pos $.end;

    has $.match;
}


role Perl::Compiler::PIL::Util::Type { }

class Perl::Compiler::PIL::Util::ConcreteType
    does Perl::Compiler::PIL::Util::Type {
    has Str $.name;
}

class Perl::Compiler::PIL::Util::TypeConjunction
    does Perl::Compiler::PIL::Util::Type {
    has Perl::Compiler::PIL::Util::Type $.left;
    has Perl::Compiler::PIL::Util::Type $.right;
}

class Perl::Compiler::PIL::Util::TypeDisjunction
    does Perl::Compiler::PIL::Util::Type {
    has Perl::Compiler::PIL::Util::Type $.left;
    has Perl::Compiler::PIL::Util::Type $.right;
}


role Perl::Compiler::PIL::Util::Context { }

class Perl::Compiler::PIL::Util::VoidContext
    does Perl::Compiler::PIL::Util::Context 
{ }

class Perl::Compiler::PIL::Util::ItemContext
    does Perl::Compiler::PIL::Util::Context {
    has Perl::Compiler::PIL::Util::Type $.type;
}

class Perl::Compiler::PIL::Util::SlurpyContext
    does Perl::Compiler::PIL::Util::Context {
    has Perl::Compiler::PIL::Util::Type @.types;
    has Bit $.unbounded;   # the last type represents a *@array of 
                           # that type if true
}


class Perl::Compiler::PIL::Util::Signature {
    has Perl::Compiler::PIL::Util::Parameter @.params;
}

class Perl::Compiler::PIL::Util::Parameter {
    has Bit $.is_invocant;
    has Bit $.is_optional;
    has Bit $.is_named;
    has Bit $.is_ref;
    has Bit $.is_rw;
    has Bit $.is_delayed;
    
    has Str $.name;
    has Perl::Compiler::PIL::Util::Context $.context;
    has Perl::Compiler::PIL::PIL $.default;
}

# vim: ft=perl6 :
