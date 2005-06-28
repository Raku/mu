
package Perl::Compiler::PIL;

use strict;
use warnings;

use Perl6::MetaModel;

use Perl::Compiler::PIL::Util;

=pod

role Perl::Compiler::PIL::PIL {
    method vtype () {...}
}

=cut

role 'Perl::Compiler::PIL::PIL' => {
    methods => {
        vtype => sub { die 'You cannot call a stub method!!' }
    }
};

=pod

class Perl::Compiler::PIL::PILNil
    does Perl::Compiler::PIL::PIL {
    method vtype () { '[Stmt]' }
}

=cut

class 'Perl::Compiler::PIL::PILNil' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        methods => {
            vtype => sub { '[Stmt]' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILNoop
    does Perl::Compiler::PIL::PIL {
    method vtype () { 'Stmt' }
}

=cut

class 'Perl::Compiler::PIL::PILNoop' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        methods => {
            vtype => sub { 'Stmt' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILExp
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'LValue';   # should be 'fail' once 'use fatal' is implemented
    }
    method vtype () { 'Expression' }
}

=cut

class 'Perl::Compiler::PIL::PILExp' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::PIL' => '$.value' ] 
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.value cannot have a vtype of LValue'
                unless ($self->get_value('$.value')->vtype() eq 'LValue');
        },
        methods => {
            vtype => sub { 'Expression' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILLit
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'Literal';
    }
    method vtype () { 'Expression' }
}

=cut

class 'Perl::Compiler::PIL::PILLit' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::PIL' => '$.value' ] 
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.value cannot have a vtype of Literal'
                unless ($self->get_value('$.value')->vtype() eq 'Literal');
        },
        methods => {
            vtype => sub { 'Expression' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILPos
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    has Util::PosRange $.pos;
}

=cut

class 'Perl::Compiler::PIL::PILPos' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [
            [ 'Perl::Compiler::PIL::PIL' => '$.value' ],
            [ 'Perl::Compiler::PIL::Util::PosRange' => '$.pos' ]            
        ]
    }
};

=pod

class Perl::Compiler::PIL::PILStmt
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'Expression';
    }
    method vtype () { 'Stmt' }
}

=cut

class 'Perl::Compiler::PIL::PILStmt' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::PIL' => '$.value' ] 
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.value cannot have a vtype of Expression'
                unless ($self->get_value('$.value')->vtype() eq 'Expression');
        },
        methods => {
            vtype => sub { 'Stmt' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILThunk
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.value;
    submethod BUILD (Perl::Compiler::PIL::PIL $.value) {
        die unless $.value.vtype eq 'Expression';
    }
    method vtype () { 'Expression' }
}

=cut

class 'Perl::Compiler::PIL::PILThunk' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::PIL' => '$.value' ] 
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.value cannot have a vtype of Expression'
                unless ($self->get_value('$.value')->vtype() eq 'Expression');
        },
        methods => {
            vtype => sub { 'Expression' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILCode
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::Util::Type $.codetype;
    has Perl::Compiler::PIL::Util::Signature $.signature;
    has PIL $.statements;
    submethod BUILD (Perl::Compiler::PIL::Util::Type $.codetype, Perl::Compiler::PIL::Util::Signature $.
signature,
                     Perl::Compiler::PIL::PIL $.statements) {
        die unless $.value.vtype eq '[Stmt]';
    }
    method vtype () { 'Expression' }
}

=cut

class 'Perl::Compiler::PIL::PILCode' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::Util::Type' => '$.codetype' ],
            [ 'Perl::Compiler::PIL::Util::Signature' => '$.signature' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.statements' ],                        
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.value cannot have a vtype of [Stmt]'
                unless ($self->get_value('$.value')->vtype() eq '[Stmt]');
        },
        methods => {
            vtype => sub { 'Expression' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILVal
    does Perl::Compiler::PIL::PIL {
    has $.value;
    method vtype () { 'Literal' }
}

=cut

class 'Perl::Compiler::PIL::PILVal' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ '$.value' ],
        methods => {
            vtype => sub { 'Literal' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILVar
    does Perl::Compiler::PIL::PIL {
    has Str $.value;
    has Perl::Compiler::PIL::Util::Pad $.pad;
    method vtype () { 'LValue' }
}

=cut

class 'Perl::Compiler::PIL::PILVar' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            '$.value',
            [ 'Perl::Compiler::PIL::Util::Pad' => '$.pad' ]
        ],
        methods => {
            vtype => sub { 'LValue' }
        }
    }  
};

=pod

class Perl::Compiler::PIL::PILStmts
    does Perl::Compiler::PIL::PIL {
    has Perl::Compiler::PIL::PIL $.head;
    has Perl::Compiler::PIL::PIL $.tail;
    submethod BUILD (Perl::Compiler::PIL::PIL $.head, Perl::Compiler::PIL::PIL $.tail) {
        die unless $.head.vtype eq 'Stmt' and $.tail.vtype eq 'Stmts';
    }
    method vtype () { 'Stmts' }
}

=cut

class 'Perl::Compiler::PIL::PILStmts' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [ 
            [ 'Perl::Compiler::PIL::PIL' => '$.head' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.tail' ],            
        ],
        init => sub {
            my ($self) = @_;
            die 'The $.head cannot have a vtype of Stmt and $.tail cannot have a vtype of Stmts'
                unless (($self->get_value('$.head')->vtype() eq 'Stmt')
                         && 
                        ($self->get_value('$.tail')->vtype() eq 'Stmts'));
        },        
        methods => {
            vtype => sub { 'Stmts' }
        }
    }  
};

=pod

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

=cut

class 'Perl::Compiler::PIL::PILApp' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [
            [ 'Perl::Compiler::PIL::Util::Context' => '$.context' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.code' ],
            [ 'Perl::Compiler::PIL::PIL' => '@.args' ],
        ],
        init => sub {
            my ($self) = @_;
            my $args = $self->get_value('@.args');
            foreach my $arg (@$args) {
                die 'The @.args cannot have a vtype of Expression'
                    unless ($_->vtype() eq 'Expression');
            }          
        },        
        methods => {
            vtype => sub { 'Expression' }
        }        
    }    
};

=pod

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

=cut

class 'Perl::Compiler::PIL::PILAssign' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [
            [ 'Perl::Compiler::PIL::PIL' => '@.lefts' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.right' ],
        ],
        init => sub {
            my ($self) = @_;
            my $lefts = $self->get_value('@.lefts');
            foreach my $left (@$lefts) {
                die 'The @.lefts cannot have a vtype of LValue'
                    unless ($_->vtype() eq 'LValue');
            }          
            die 'The $.right cannot have a vtype of Expression'
                unless ($self->get_value('$.right')->vtype eq 'Expression');
        },        
        methods => {
            vtype => sub { 'LValue' }
        }        
    }    
};

=pod

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

=cut

class 'Perl::Compiler::PIL::PILBind' => {
    does => [ 'Perl::Compiler::PIL::PIL' ],
    class => {
        attrs => [
            [ 'Perl::Compiler::PIL::PIL' => '@.lefts' ],
            [ 'Perl::Compiler::PIL::PIL' => '$.right' ],
        ],
        init => sub {
            my ($self) = @_;
            my $lefts = $self->get_value('@.lefts');
            foreach my $left (@$lefts) {
                die 'The @.lefts cannot have a vtype of LValue'
                    unless ($_->vtype() eq 'LValue');
            }          
            die 'The $.right cannot have a vtype of Expression'
                unless ($self->get_value('$.right')->vtype eq 'Expression');
        },        
        methods => {
            vtype => sub { 'LValue' }
        }        
    }    
};

1;