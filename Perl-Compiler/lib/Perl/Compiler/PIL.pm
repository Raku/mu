module Perl::Compiler::PIL;

#use fatal;

role PIL {
    method vtype () {...}
}

class PILNil
    does PIL {
    method vtype () { '[Stmt]' }
}

class PILNoop
    does PIL {
    method vtype () { 'Stmt' }
}

class PILExp
    does PIL {
    has PIL $.value;
    submethod BUILD (PIL $.value) {
        fail unless $.value.vtype eq 'LValue';
    }
    method vtype () { 'Expression' }
}

class PILLit
    does PIL {
    has PIL $.value;
    submethod BUILD (PIL $.value) { 
        fail unless $.value.vtype eq 'Literal';
    }
    method vtype () { 'Expression' }
}

class PILPos
    does PIL {
    has PIL $.value;
    has Util::PosRange $.pos;
}

class PILStmt
    does PIL {
    has PIL $.value;
    submethod BUILD (PIL $.value) {
        fail unless $.value.vtype eq 'Expression';
    }
    method vtype () { 'Stmt' }
}

class PILThunk
    does PIL {
    has PIL $.value;
    submethod BUILD (PIL $.value) {
        fail unless $.value.vtype eq 'Expression';
    }
    method vtype () { 'Expression' }
}

class PILCode
    does PIL {
    has Util::Type $.codetype;
    has Util::Signature $.signature;
    has PIL $.statements;
    submethod BUILD (Util::Type $.codetype, Util::Signature $.signature, 
                     PIL $.statements) {
        fail unless $.value.vtype eq '[Stmt]';
    }
    method vtype () { 'Expression' }
}

class PILVal
    does PIL {
    has $.value;
    method vtype () { 'LValue' }
}

class PILVar
    does PIL {
    has Str $.value;
    has Util::Pad $.pad;
    method vtype () { 'LValue' }
}

class PILStmts
    does PIL {
    has PIL $.head;
    has PIL $.tail;
    submethod BUILD (PIL $.head, PIL $.tail) {
        fail unless $.head.vtype eq 'Stmt' and $.tail.vtype eq 'Stmts';
    }
    method vtype () { 'Stmts' }
}

class PILApp
    does PIL {
    has Util::Context $.context;
    has PIL $.code;
    has PIL @.args;
    submethod BUILD ($.context, PIL $.code, PIL @.args) {
        fail unless all($.code.vtype, @.args).vtype eq 'Expression';
    }
    method vtype () { 'Expression' }
}

class PILAssign
    does PIL {
    has PIL @.lefts;
    has PIL $.right;
    submethod BUILD (PIL @.lefts, PIL $.right) {
        fail unless all(@.lefts).vtype eq 'LValue'
                and $.right.vtype eq 'Expression';
    }
    method vtype () { 'LValue' }
}

class PILBind
    does PIL {
    has PIL @.lefts;
    has PIL $.right;
    submethod BUILD (PIL @.lefts, PIL $.right) {
        fail unless all(@.lefts).vtype eq 'LValue'
                and $.right.vtype eq 'Expression';
    }
    method vtype () { 'LValue' }
}

# vim: ft=perl6 :
