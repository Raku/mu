module Parse::Rule::AST;

# XXX these are implied "our class", not "class *" as pugs implements
# Therefore, in pugs, these pollute the global namepace.  I'd rather
# await "our class" support than fully-qualify them.

# === Context-free languages ===

# The reason the classes are rw is so that it is possible to make
# recursive structures.
class Literal is rw { 
    has $.string;
}
class Union is rw {
    has $.left;
    has $.right;
}
class Concat is rw {
    has $.left;
    has $.right;
}

# === Backtracking controls ===
class Mark is rw {
    has $.exp;
}
class Cut is rw {
    has $.mark;
}

# vim: ft=perl6 :
