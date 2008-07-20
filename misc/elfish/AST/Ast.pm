package IRx1;
class Base {
    has $.match;
    has $.notes;
}
class CompUnit is Base {
    has $.statements;
    has $.filename;
}
class Block is Base {
    has $.statements;
}
class Use is Base {
    has $.kind;
    has $.module_name;
    has $.expr;
}
class PackageDecl is Base {
    has $.scope;
    has $.plurality;
    has $.kind;
    has $.name;
    has $.traits;
    has $.block;
}
class MethodDecl is Base {
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    has $.sigil;
    has $.postcircumfix;
}
class SubDecl is Base {
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
}
class MacroDecl is Base {
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
}
class VarDecl is Base {
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.var;
    has $.postcircumfix;
    has $.traits;
    has $.default_op;
    has $.default_expr;
}
class Var is Base {
    has $.sigil;
    has $.twigil;
    has $.name;
}
class Trait is Base {
    has $.verb;
    has $.expr;
}
class ClosureTrait is Base {
    has $.kind;
    has $.block;
}
class ModuleName is Base {
    has $.name;
    has $.pairs;
}
class PathName is Base {
    has $.path;
}
class SubName is Base {
    has $.category;
    has $.pairs;
    has $.desigilname;
    has $.signature;
}
class ShapedParamName is Base {
    has $.ident;
    has $.postcircumfix;
}
class Call is Base {
    has $.invocant;
    has $.capture;
}
class Apply is Base {
    has $.function;
    has $.capture;
}
class Hyper is Base {
    has $.operator;
    has $.capture;
}
class Capture is Base {
    has $.arguments;
    has $.invocant;
}
class MultiSig is Base {
    has $.signatures;
}
class Signature is Base {
    has $.parameters;
    has $.return_type;
}
class Parameter is Base {
    has $.type_constraints;
    has $.quant;
    has $.param_var;
    has $.ident;
    has $.traits;
    has $.post_constraints;
    has $.default_expr;
}
class TypeConstraint is Base {
    has $.value;
    has $.where_expr;
}
class PostConstraint is Base {
    has $.multisig;
    has $.where_expr;
}
class ParamVar is Base {
    has $.sigil;
    has $.twigil;
    has $.name;
}
class Undef is Base {
}
class NumInt is Base {
    has $.text;
    has $.base;
}
class NumDec is Base {
    has $.intpart;
    has $.fracpart;
    has $.exp;
}
class NumRad is Base {
    has $.radix;
    has $.intpart;
    has $.fracpart;
    has $.base;
    has $.exp;
}
class Array is Base {
    has $.array;
}
class Hash is Base {
    has $.hash;
}
class Pair is Base {
    has $.key;
    has $.value;
}
class Type is Base {
    has $.typename;
}
class Rx is Base {
    has $.pat;
    has $.modifiers;
}
class Buf is Base {
    has $.buf;
}
class For is Base {
    has $.expr;
    has $.block;
}
class Cond is Base {
    has $.clauses;
    has $.default;
    has $.invert_first_test;
}
class Loop is Base {
    has $.pretest;
    has $.block;
    has $.posttest;
    has $.label;
}
class Given is Base {
    has $.expr;
    has $.block;
}
class When is Base {
    has $.expr;
    has $.block;
}
class Label is Base {
    has $.labels;
    has $.statement;
}
class RegexDef is Base {
    has $.ident;
    has $.pattern;
}
class Regex is Base {
    has $.patterns;
}
class RxFirst is Base {
    has $.patterns;
}
class RxEvery is Base {
    has $.patterns;
}
class RxSubmatch is Base {
    has $.patterns;
}
class RxAny is Base {
    has $.patterns;
}
class RxAll is Base {
    has $.patterns;
}
class RxSequence is Base {
    has $.patterns;
}
class RxQuantifiedAtom is Base {
    has $.atom;
    has $.quantifier;
}
class RxBackslash is Base {
    has $.char;
}
class RxAssertion is Base {
    has $.ident;
}
class RxModInternal is Base {
    has $.mod;
}
class RxCapture is Base {
    has $.pattern;
}
class RxGroup is Base {
    has $.pattern;
}
class RxBlock is Base {
    has $.block;
}
class RxBind is Base {
    has $.var;
    has $.binding;
}
class RxLiteral is Base {
    has $.text;
    has $.quote;
}
class RxSymbol is Base {
    has $.symbol;
}
