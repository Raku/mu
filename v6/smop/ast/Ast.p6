class Label {
    has $.identifier;
    has $.stmt;
}
class Block {
    has @.stmts;
}
class IntegerConstant {
    has $.value;
}
class StringConstant {
    has $.value;
}
class Call {
    has $.capture;
    has $.identifier;
}
class Capture {
    has $.ctx
    has %.named;
    has $.invocant;
    has $.positional;
}
class Goto {
    has $.label;
    has $.stmt;
}
