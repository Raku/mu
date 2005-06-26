
=kwid

= NAME

Perl::Compiler::PIL - Specifies PIL from pugs/src/Pugs/Compiler.hs in Perl 6's terms

= DESCRIPTION

=cut

#use fatal;

use Perl::Compiler::PIL::Util;

role Perl::Compiler::PIL::PIL {
    method vtype () {...}
}

=kwid

== PIL

The type class for PIL nodes.  vtype() returns a string that represents the 
node type, which are checked against each other for agreement at BUILD time
for the below classes.

=cut

# The types given below, although Haskellesque, are not pure functions.  They only
# represent transformations between PIL node types; out-of-band data is not listed.

# PILNil :: [Stmt]
class Perl::Compiler::PIL::PILNil
    does Perl::Compiler::PIL::PIL {
    method vtype () { '[Stmt]' }
}

# PILNoop :: Stmt
class Perl::Compiler::PIL::PILNoop
    does Perl::Compiler::PIL::PIL {
    method vtype () { 'Stmt' }
}

# PILExp :: LValue -> Expression
class Perl::Compiler::PIL::PILExp
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'LValue';   # should be 'fail' once 'use fatal' is implemented
    }
    method vtype () { 'Expression' }
}

# PILLit :: Literal -> Expression
class Perl::Compiler::PIL::PILLit
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) { 
        die unless $.value.vtype eq 'Literal';
    }
    method vtype () { 'Expression' }
}

# PILPos :: a -> a
class Perl::Compiler::PIL::PILPos
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    has Util::PosRange $.pos;

    method vtype () { .value.vtype }
}

# PILStmt :: Expression -> Stmt
class Perl::Compiler::PIL::PILStmt
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'Expression';
    }
    method vtype () { 'Stmt' }
}

# PILThunk :: Stmt -> Expression
class Perl::Compiler::PIL::PILThunk
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'Stmt';
    }
    method vtype () { 'Expression' }
}

# PILCode :: [Stmt] -> Expression
class Perl::Compiler::PIL::PILCode
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::Util::Type $.codetype;
    has Perl::Compiler::PIL::Util::Signature $.signature;
    has PIL $.statements;
    submethod BUILD (Perl::Compiler::PIL::Util::Type $.codetype, Perl::Compiler::PIL::Util::Signature $.signature, 
                     Perl::Compiler::PIL::PIL $.statements) {
        die unless $.value.vtype eq '[Stmt]';
    }
    method vtype () { 'Expression' }
}

# PILVal :: Literal
class Perl::Compiler::PIL::PILVal
    does Perl::Compiler::PIL::PIL {
    has $.value;
    method vtype () { 'Literal' }
}

# PILVar :: LValue
class Perl::Compiler::PIL::PILVar
    does Perl::Compiler::PIL::PIL {
    has Str $.value;
    has Perl::Compiler::PIL::Util::Pad $.pad;
    method vtype () { 'LValue' }
}

# PILStmts :: Stmt -> [Stmt] -> [Stmt]
class Perl::Compiler::PIL::PILStmts
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.head;
    has Perl::Compiler::PIL::PIL $.tail;
    submethod BUILD (Perl::Compiler::PIL::PIL $.head, Perl::Compiler::PIL::PIL $.tail) {
        die unless $.head.vtype eq 'Stmt' and $.tail.vtype eq '[Stmt]';
    }
    method vtype () { '[Stmt]' }
}

# PILApp :: Expression -> [Expression] -> Expression
class Perl::Compiler::PIL::PILApp
    does Perl::Compiler::PIL::PIL {
    has Util::Context $.context;
    has Perl::Compiler::PIL::PIL $.code;
    has Perl::Compiler::PIL::PIL @.args;
    submethod BUILD ($.context, Perl::Compiler::PIL::PIL $.code, Perl::Compiler::PIL::PIL @.args) {
        die unless all($.code.vtype, @.args).vtype eq 'Expression';
    }
    method vtype () { 'Expression' }
}

# PILAssign :: LValue -> Expression -> LValue
class Perl::Compiler::PIL::PILAssign
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL @.lefts;
    has Perl::Compiler::PIL::PIL $.right;
    submethod BUILD (Perl::Compiler::PIL::PIL @.lefts, Perl::Compiler::PIL::PIL $.right) {
        die unless all(@.lefts).vtype eq 'LValue'
                and $.right.vtype eq 'Expression';
    }
    method vtype () { 'LValue' }
}

# PILBind :: LValue -> LValue -> LValue
class Perl::Compiler::PIL::PILBind
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL @.lefts;
    has Perl::Compiler::PIL::PIL $.right;
    submethod BUILD (Perl::Compiler::PIL::PIL @.lefts, Perl::Compiler::PIL::PIL $.right) {
        die unless all(@.lefts).vtype eq 'LValue'
                and $.right.vtype eq 'Expression';
    }
    method vtype () { 'LValue' }
}

# vim: ft=perl6 :
