package Grammar::Perl6;
use base 'Pugs::Grammar::Base', 'Pugs::Grammar::Rule', 'Grammar::Perl6Init';
use Runtime::RuleCompiler;
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;

${'immediate_statement_rule'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
       capture( '*quantifier*',
           optional(
                 sub{ 
                     $grammar->p6ws( @_ );
                 }
           )
       )
,       concat( 
           alternation( \@Grammar::Perl6::statements )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
               )
           )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return  $_[0]->()  } };
                 }
             )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'immediate_statement_rule'} = ${'immediate_statement_rule'}->code();
${'grammar'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
       capture( '*quantifier*',
           greedy_star(
                 capture( 'immediate_statement_rule', 
                   sub{ 
                       $grammar->immediate_statement_rule( @_ );
                   }
                 )
           )
       )
,         abort(
             sub {
                 return { bool => 1, tail => $_[0], return => sub { return  $_[0]->()  } };
             }
         )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'grammar'} = ${'grammar'}->code();
${'indirect_object'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'varscalar', 
           sub{ 
               $grammar->varscalar( @_ );
           }
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   capture( 'p6ws', 
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
                   )
             )
         )
,         concat( 
             constant( q!:! )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return  $_[0]->{varscalar}  } };
                 }
             )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'indirect_object'} = ${'indirect_object'}->code();
${'rule_decl'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(r) )
,       concat( 
           constant( q(u) )
,         concat( 
             constant( q(l) )
,           concat( 
               constant( q(e) )
,             concat( 
                 capture( 'p6ws', 
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
                 )
,               concat( 
                   capture( 'ident', 
                     sub{ 
                         $grammar->ident( @_ );
                     }
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             capture( 'p6ws', 
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                             )
                       )
                   )
,                   concat( 
                       constant( q!{! )
,                     concat( 
                         capture( 'rule', 
                           sub{ 
                               $grammar->rule( @_ );
                           }
                         )
,                       concat( 
                           constant( q!}! )
,                           abort(
                               sub {
                                   return { bool => 1, tail => $_[0], return => sub { return { rule_decl =>  $_[0]->()  ,} } };
                               }
                           )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'rule_decl'} = ${'rule_decl'}->code();
${'grammar_name'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(g) )
,       concat( 
           constant( q(r) )
,         concat( 
             constant( q(a) )
,           concat( 
               constant( q(m) )
,             concat( 
                 constant( q(m) )
,               concat( 
                   constant( q(a) )
,                 concat( 
                     constant( q(r) )
,                   concat( 
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
,                     concat( 
                         capture( 'ident', 
                           sub{ 
                               $grammar->ident( @_ );
                           }
                         )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   capture( 'p6ws', 
                                     sub{ 
                                         $grammar->p6ws( @_ );
                                     }
                                   )
                             )
                         )
,                         concat( 
                             constant( q!;! )
,                             abort(
                                 sub {
                                     return { bool => 1, tail => $_[0], return => sub { return { grammar_name =>  $_[0]->()  ,} } };
                                 }
                             )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'grammar_name'} = ${'grammar_name'}->code();
${'condition_rule'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(i) )
,                 constant( q(f) )
             )
,             concat( 
                 constant( q(u) )
,               concat( 
                   constant( q(n) )
,                 concat( 
                     constant( q(l) )
,                   concat( 
                       constant( q(e) )
,                     concat( 
                         constant( q(s) )
,                         constant( q(s) )
                     )
                   )
                 )
               )
             )
           ] )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!(! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'condition', 
                     capture( 'term1', 
                       sub{ 
                           $grammar->term1( @_ );
                       }
                     )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!)! )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                         )
                     )
,                     concat( 
                         capture( 'then', 
                             capture( 'block', 
                               sub{ 
                                   $grammar->block( @_ );
                               }
                             )
                         )
,                         abort(
                             sub {
                                 return { bool => 1, tail => $_[0], return => sub { return { condition =>  $_[0]->()  } } };
                             }
                         )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'condition_rule'} = ${'condition_rule'}->code();
${'meth_call_term'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'class', 
             capture( 'ident', 
               sub{ 
                   $grammar->ident( @_ );
               }
             )
         )
,       concat( 
           constant( q!.! )
,         concat( 
             capture( 'meth', 
                 capture( 'word', 
                   sub{ 
                       $grammar->word( @_ );
                   }
                 )
             )
,           concat( 
               constant( q!(! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                   )
               )
,               concat( 
                   capture( 'params', 
                     capture( '*quantifier*',
                         optional(
                               capture( 'list', 
                                 sub{ 
                                     $grammar->list( @_ );
                                 }
                               )
                         )
                     )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                       )
                   )
,                   concat( 
                       constant( q!)! )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                           )
                       )
,                         abort(
                             sub {
                                 return { bool => 1, tail => $_[0], return => sub { return { meth_call_term =>  $_[0]->()  } } };
                             }
                         )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'meth_call_term'} = ${'meth_call_term'}->code();
${'meth_call_statement'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'class', 
             capture( 'ident', 
               sub{ 
                   $grammar->ident( @_ );
               }
             )
         )
,       concat( 
           constant( q!.! )
,         concat( 
             capture( 'meth', 
                 capture( 'word', 
                   sub{ 
                       $grammar->word( @_ );
                   }
                 )
             )
,           concat( 
               constant( q!(! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                   )
               )
,               concat( 
                   capture( 'params', 
                     capture( '*quantifier*',
                         optional(
                               capture( 'list', 
                                 sub{ 
                                     $grammar->list( @_ );
                                 }
                               )
                         )
                     )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                       )
                   )
,                   concat( 
                       constant( q!)! )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                           )
                       )
,                       concat( 
                           constant( q!;! )
,                           abort(
                               sub {
                                   return { bool => 1, tail => $_[0], return => sub { return { meth_call =>  $_[0]->()  } } };
                               }
                           )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'meth_call_statement'} = ${'meth_call_statement'}->code();
${'sub_call_term'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'name', 
             capture( 'ident', 
               sub{ 
                   $grammar->ident( @_ );
               }
             )
         )
,       concat( 
           constant( q!(! )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
               )
           )
,           concat( 
               capture( 'params', 
                 capture( '*quantifier*',
                     optional(
                           capture( 'list', 
                             sub{ 
                                 $grammar->list( @_ );
                             }
                           )
                     )
                 )
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                   )
               )
,               concat( 
                   constant( q!)! )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                       )
                   )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { sub_call_term =>  $_[0]->()  } } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_call_term'} = ${'sub_call_term'}->code();
${'sub_call_statement'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'name', 
             capture( 'ident', 
               sub{ 
                   $grammar->ident( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!(! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'params', 
                   capture( '*quantifier*',
                       optional(
                             capture( 'list', 
                               sub{ 
                                   $grammar->list( @_ );
                               }
                             )
                       )
                   )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!)! )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                         )
                     )
,                     concat( 
                         constant( q!;! )
,                         abort(
                             sub {
                                 return { bool => 1, tail => $_[0], return => sub { return { sub_call =>  $_[0]->()  } } };
                             }
                         )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_call_statement'} = ${'sub_call_statement'}->code();
${'access_hashref_element'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'varscalar', 
               sub{ 
                   $grammar->varscalar( @_ );
               }
             )
         )
,       concat( 
           constant( q!{! )
,         concat( 
             capture( 'key', 
                 capture( 'term1', 
                   sub{ 
                       $grammar->term1( @_ );
                   }
                 )
             )
,           concat( 
               constant( q!}! )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return { access_hashref_element =>  $_[0]->()  } } };
                   }
               )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'access_hashref_element'} = ${'access_hashref_element'}->code();
${'access_hash_element'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'varhash', 
               sub{ 
                   $grammar->varhash( @_ );
               }
             )
         )
,       concat( 
           constant( q!{! )
,         concat( 
             capture( 'key', 
                 capture( 'term1', 
                   sub{ 
                       $grammar->term1( @_ );
                   }
                 )
             )
,           concat( 
               constant( q!}! )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return { access_hash_element =>  $_[0]->()  } } };
                   }
               )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'access_hash_element'} = ${'access_hash_element'}->code();
${'assign_hash_to_scalar'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'varscalar', 
               sub{ 
                   $grammar->varscalar( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!=! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'value', 
                     capture( 'varhash', 
                       sub{ 
                           $grammar->varhash( @_ );
                       }
                     )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!;! )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { assign_hash_to_scalar =>  $_[0]->()  } } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'assign_hash_to_scalar'} = ${'assign_hash_to_scalar'}->code();
${'assign_slurp_to_variable'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'variable', 
               sub{ 
                   $grammar->variable( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!=! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 constant( q(s) )
,               concat( 
                   constant( q(l) )
,                 concat( 
                     constant( q(u) )
,                   concat( 
                       constant( q(r) )
,                     concat( 
                         constant( q(p) )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( @_ );
                                   }
                             )
                         )
,                         concat( 
                             capture( 'value', 
                                 capture( 'term1', 
                                   sub{ 
                                       $grammar->term1( @_ );
                                   }
                                 )
                             )
,                           concat( 
                             capture( '*quantifier*',
                                 optional(
                                       sub{ 
                                           $grammar->p6ws( @_ );
                                       }
                                 )
                             )
,                             concat( 
                                 constant( q!;! )
,                                 abort(
                                     sub {
                                         return { bool => 1, tail => $_[0], return => sub { return { slurp =>  $_[0]->()  } } };
                                     }
                                 )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'assign_slurp_to_variable'} = ${'assign_slurp_to_variable'}->code();
${'assign_open_to_variable'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'variable', 
               sub{ 
                   $grammar->variable( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!=! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 constant( q(o) )
,               concat( 
                   constant( q(p) )
,                 concat( 
                     constant( q(e) )
,                   concat( 
                       constant( q(n) )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                           )
                       )
,                       concat( 
                           capture( 'value', 
                               capture( 'term1', 
                                 sub{ 
                                     $grammar->term1( @_ );
                                 }
                               )
                           )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     sub{ 
                                         $grammar->p6ws( @_ );
                                     }
                               )
                           )
,                           concat( 
                               constant( q!;! )
,                               abort(
                                   sub {
                                       return { bool => 1, tail => $_[0], return => sub { return { _open =>  $_[0]->()  } } };
                                   }
                               )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'assign_open_to_variable'} = ${'assign_open_to_variable'}->code();
${'assign'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'variable', 
             capture( 'term1', 
               sub{ 
                   $grammar->term1( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!=! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'value', 
                     capture( 'term1', 
                       sub{ 
                           $grammar->term1( @_ );
                       }
                     )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!;! )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { assign =>  $_[0]->()  } } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'assign'} = ${'assign'}->code();
${'sub_call'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'name', 
             capture( 'ident', 
               sub{ 
                   $grammar->ident( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
             )
         )
,         concat( 
             constant( q!(! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'params', 
                   capture( '*quantifier*',
                       optional(
                             capture( 'list', 
                               sub{ 
                                   $grammar->list( @_ );
                               }
                             )
                       )
                   )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!)! )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                         )
                     )
,                     concat( 
                         constant( q!;! )
,                         abort(
                             sub {
                                 return { bool => 1, tail => $_[0], return => sub { return { sub_call =>  $_[0]->()  } } };
                             }
                         )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_call'} = ${'sub_call'}->code();
${'_push'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(p) )
,               concat( 
                   constant( q(u) )
,                 concat( 
                     constant( q(s) )
,                     constant( q(h) )
                 )
               )
             )
,             concat( 
                 constant( q(u) )
,               concat( 
                   constant( q(n) )
,                 concat( 
                     constant( q(s) )
,                   concat( 
                       constant( q(h) )
,                     concat( 
                         constant( q(i) )
,                       concat( 
                           constant( q(f) )
,                           constant( q(t) )
                       )
                     )
                   )
                 )
               )
             )
           ] )
         )
,       concat( 
           capture( 'p6ws', 
             sub{ 
                 $grammar->p6ws( @_ );
             }
           )
,         concat( 
             capture( 'variable', 
               sub{ 
                   $grammar->variable( @_ );
               }
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 constant( q!,! )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           capture( 'p6ws', 
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                           )
                     )
                 )
,                 concat( 
                     capture( 'code', 
                       capture( '*quantifier*',
                           non_greedy_star(
                                 sub{ 
                                     $grammar->any( @_ );
                                 }
                           )
                       )
                     )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               capture( 'p6ws', 
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                               )
                         )
                     )
,                     concat( 
                         constant( q!;! )
,                         abort(
                             sub {
                                 return { bool => 1, tail => $_[0], return => sub { return { _push =>  $_[0]->()  ,} } };
                             }
                         )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_push'} = ${'_push'}->code();
${'pod'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q!=! )
,       concat( 
           alternation( [
             concat( 
                 constant( q(p) )
,               concat( 
                   constant( q(o) )
,                   constant( q(d) )
               )
             )
,             alternation( [
               concat( 
                   constant( q(h) )
,                 concat( 
                     constant( q(e) )
,                   concat( 
                       constant( q(a) )
,                     concat( 
                         constant( q(d) )
,                         constant( q(1) )
                     )
                   )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q(k) )
,                   concat( 
                       constant( q(w) )
,                     concat( 
                         constant( q(i) )
,                         constant( q(d) )
                     )
                   )
                 )
,                 concat( 
                     constant( q(f) )
,                   concat( 
                       constant( q(o) )
,                       constant( q(r) )
                   )
                 )
               ] )
             ] )
           ] )
,         concat( 
           capture( '*quantifier*',
               non_greedy_star(
                     sub{ 
                         $grammar->any( @_ );
                     }
               )
           )
,           concat( 
               constant( q!=! )
,             concat( 
                 constant( q(c) )
,               concat( 
                   constant( q(u) )
,                   constant( q(t) )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'pod'} = ${'pod'}->code();
${'use_v6'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(u) )
,       concat( 
           constant( q(s) )
,         concat( 
             constant( q(e) )
,           concat( 
               sub{ 
                   $grammar->p6ws( @_ );
               }
,             concat( 
                 constant( q(v) )
,               concat( 
                   constant( q(6) )
,                 concat( 
                     constant( q!-! )
,                   concat( 
                       constant( q(p) )
,                     concat( 
                         constant( q(u) )
,                       concat( 
                           constant( q(g) )
,                         concat( 
                             constant( q(s) )
,                           concat( 
                             capture( '*quantifier*',
                                 optional(
                                       sub{ 
                                           $grammar->p6ws( @_ );
                                       }
                                 )
                             )
,                               constant( q!;! )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'use_v6'} = ${'use_v6'}->code();
${'require'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(r) )
,       concat( 
           constant( q(e) )
,         concat( 
             constant( q(q) )
,           concat( 
               constant( q(u) )
,             concat( 
                 constant( q(i) )
,               concat( 
                   constant( q(r) )
,                 concat( 
                     constant( q(e) )
,                   concat( 
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
,                     concat( 
                         capture( 'ident', 
                           sub{ 
                               $grammar->ident( @_ );
                           }
                         )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( @_ );
                                   }
                             )
                         )
,                         concat( 
                             constant( q!;! )
,                             abort(
                                 sub {
                                     return { bool => 1, tail => $_[0], return => sub { 
		eval 'require '. $_[0]->() ->[2]{ident}[0]{ident};
		return { require_bareword =>  $_[0]->()  ,} 
	} };
                                 }
                             )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'require'} = ${'require'}->code();
${'use_rule'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(u) )
,       concat( 
           constant( q(s) )
,         concat( 
             constant( q(e) )
,           concat( 
               sub{ 
                   $grammar->p6ws( @_ );
               }
,             concat( 
                 capture( 'ident', 
                   sub{ 
                       $grammar->ident( @_ );
                   }
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     constant( q!;! )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { 
		eval 'use '. $_[0]->() ->[2]{ident}[0]{ident};
		return { use_bareword =>  $_[0]->()  ,} 
	} };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'use_rule'} = ${'use_rule'}->code();
${'term1'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       alternation( \@Grammar::Perl6::terms )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'term1'} = ${'term1'}->code();
${'list'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
       capture( '*quantifier*',
           greedy_star(
                 concat( 
                     capture( 'term1', 
                       sub{ 
                           $grammar->term1( @_ );
                       }
                     )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                         )
                     )
,                     concat( 
                         constant( q!,! )
,                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                           )
                       )
                     )
                   )
                 )
           )
       )
,       capture( '*quantifier*',
           optional(
                 capture( 'term1', 
                   sub{ 
                       $grammar->term1( @_ );
                   }
                 )
           )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'list'} = ${'list'}->code();
${'block'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q!{! )
,       concat( 
           capture( 'list', 
             capture( '*quantifier*',
                 greedy_star(
                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( @_ );
                                   }
                             )
                         )
,                           alternation( \@Grammar::Perl6::statements )
                       )
                 )
             )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
               )
           )
,           concat( 
               constant( q!}! )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return { block =>  $_[0]->{list}  ,} } };
                   }
               )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'block'} = ${'block'}->code();
${'macro_decl'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(m) )
,       concat( 
           constant( q(a) )
,         concat( 
             constant( q(c) )
,           concat( 
               constant( q(r) )
,             concat( 
                 constant( q(o) )
,               concat( 
                   sub{ 
                       $grammar->p6ws( @_ );
                   }
,                 concat( 
                     capture( 'prefix', 
                         capture( 'word', 
                           sub{ 
                               $grammar->word( @_ );
                           }
                         )
                     )
,                   concat( 
                       constant( q!:! )
,                     concat( 
                         constant( q!<! )
,                       concat( 
                           capture( 'id', 
                             capture( '*quantifier*',
                                 non_greedy_star(
                                       sub{ 
                                           $grammar->any( @_ );
                                       }
                                 )
                             )
                           )
,                         concat( 
                             constant( q!>! )
,                           concat( 
                             capture( '*quantifier*',
                                 optional(
                                       sub{ 
                                           $grammar->p6ws( @_ );
                                       }
                                 )
                             )
,                             concat( 
                                 constant( q!(! )
,                               concat( 
                                 capture( '*quantifier*',
                                     optional(
                                           sub{ 
                                               $grammar->p6ws( @_ );
                                           }
                                     )
                                 )
,                                 concat( 
                                   capture( '*quantifier*',
                                       optional(
                                             capture( 'list', 
                                               sub{ 
                                                   $grammar->list( @_ );
                                               }
                                             )
                                       )
                                   )
,                                   concat( 
                                     capture( '*quantifier*',
                                         optional(
                                               sub{ 
                                                   $grammar->p6ws( @_ );
                                               }
                                         )
                                     )
,                                     concat( 
                                         constant( q!)! )
,                                       concat( 
                                         capture( '*quantifier*',
                                             optional(
                                                   sub{ 
                                                       $grammar->p6ws( @_ );
                                                   }
                                             )
                                         )
,                                         concat( 
                                             constant( q(i) )
,                                           concat( 
                                               constant( q(s) )
,                                             concat( 
                                                 sub{ 
                                                     $grammar->p6ws( @_ );
                                                 }
,                                               concat( 
                                                   constant( q(p) )
,                                                 concat( 
                                                     constant( q(a) )
,                                                   concat( 
                                                       constant( q(r) )
,                                                     concat( 
                                                         constant( q(s) )
,                                                       concat( 
                                                           constant( q(e) )
,                                                         concat( 
                                                             constant( q(d) )
,                                                           concat( 
                                                             capture( '*quantifier*',
                                                                 optional(
                                                                       sub{ 
                                                                           $grammar->p6ws( @_ );
                                                                       }
                                                                 )
                                                             )
,                                                             concat( 
                                                                 constant( q!(! )
,                                                               concat( 
                                                                 capture( '*quantifier*',
                                                                     optional(
                                                                           sub{ 
                                                                               $grammar->p6ws( @_ );
                                                                           }
                                                                     )
                                                                 )
,                                                                 concat( 
                                                                     constant( q!/! )
,                                                                   concat( 
                                                                     capture( '*quantifier*',
                                                                         optional(
                                                                               sub{ 
                                                                                   $grammar->p6ws( @_ );
                                                                               }
                                                                         )
                                                                     )
,                                                                     concat( 
                                                                         capture( 'rule', 
                                                                           sub{ 
                                                                               $grammar->rule( @_ );
                                                                           }
                                                                         )
,                                                                       concat( 
                                                                         capture( '*quantifier*',
                                                                             optional(
                                                                                   sub{ 
                                                                                       $grammar->p6ws( @_ );
                                                                                   }
                                                                             )
                                                                         )
,                                                                         concat( 
                                                                             constant( q!/! )
,                                                                           concat( 
                                                                             capture( '*quantifier*',
                                                                                 optional(
                                                                                       sub{ 
                                                                                           $grammar->p6ws( @_ );
                                                                                       }
                                                                                 )
                                                                             )
,                                                                             concat( 
                                                                                 constant( q!)! )
,                                                                               concat( 
                                                                                 capture( '*quantifier*',
                                                                                     optional(
                                                                                           sub{ 
                                                                                               $grammar->p6ws( @_ );
                                                                                           }
                                                                                     )
                                                                                 )
,                                                                                 concat( 
                                                                                     capture( 'code', 
                                                                                       sub{ 
                                                                                           $grammar->code( @_ );
                                                                                       }
                                                                                     )
,                                                                                     abort(
                                                                                         sub {
                                                                                             return { bool => 1, tail => $_[0], return => sub {
	 eval Emitter::Perl5::emit({macro =>  $_[0]->() });
	 return { macro =>  $_[0]->()  ,}
	} };
                                                                                         }
                                                                                     )
                                                                                 )
                                                                               )
                                                                             )
                                                                           )
                                                                         )
                                                                       )
                                                                     )
                                                                   )
                                                                 )
                                                               )
                                                             )
                                                           )
                                                         )
                                                       )
                                                     )
                                                   )
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'macro_decl'} = ${'macro_decl'}->code();
${'empty_list'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q!(! )
,       concat( 
           constant( q!)! )
,           abort(
               sub {
                   return { bool => 1, tail => $_[0], return => sub { return { empty_list =>  $_[0]->()  } } };
               }
           )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'empty_list'} = ${'empty_list'}->code();
${'_open'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           concat( 
               constant( q(o) )
,             concat( 
                 constant( q(p) )
,               concat( 
                   constant( q(e) )
,                   constant( q(n) )
               )
             )
           )
         )
,       concat( 
           capture( 'p6ws', 
             sub{ 
                 $grammar->p6ws( @_ );
             }
           )
,         concat( 
             capture( 'varscalar', 
               sub{ 
                   $grammar->varscalar( @_ );
               }
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 constant( q!;! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { _open =>  $_[0]->() , } } };
                     }
                 )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_open'} = ${'_open'}->code();
${'_print_with_fh'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(p) )
,               concat( 
                   constant( q(r) )
,                 concat( 
                     constant( q(i) )
,                   concat( 
                       constant( q(n) )
,                       constant( q(t) )
                   )
                 )
               )
             )
,             alternation( [
               concat( 
                   constant( q(s) )
,                 concat( 
                     constant( q(a) )
,                     constant( q(y) )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q(w) )
,                   concat( 
                       constant( q(a) )
,                     concat( 
                         constant( q(r) )
,                         constant( q(n) )
                     )
                   )
                 )
,                 concat( 
                     constant( q(d) )
,                   concat( 
                       constant( q(i) )
,                       constant( q(e) )
                   )
                 )
               ] )
             ] )
           ] )
         )
,       concat( 
           capture( 'p6ws', 
             sub{ 
                 $grammar->p6ws( @_ );
             }
           )
,         concat( 
             capture( 'indirect_object', 
               sub{ 
                   $grammar->indirect_object( @_ );
               }
             )
,           concat( 
               capture( 'p6ws', 
                 sub{ 
                     $grammar->p6ws( @_ );
                 }
               )
,             concat( 
                 capture( 'list', 
                   sub{ 
                       $grammar->list( @_ );
                   }
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           capture( 'p6ws', 
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                           )
                     )
                 )
,                 concat( 
                     constant( q!;! )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { _print_with_fh =>  $_[0]->()  ,} } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_print_with_fh'} = ${'_print_with_fh'}->code();
${'_print'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(p) )
,               concat( 
                   constant( q(r) )
,                 concat( 
                     constant( q(i) )
,                   concat( 
                       constant( q(n) )
,                       constant( q(t) )
                   )
                 )
               )
             )
,             alternation( [
               concat( 
                   constant( q(s) )
,                 concat( 
                     constant( q(a) )
,                     constant( q(y) )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q(w) )
,                   concat( 
                       constant( q(a) )
,                     concat( 
                         constant( q(r) )
,                         constant( q(n) )
                     )
                   )
                 )
,                 concat( 
                     constant( q(d) )
,                   concat( 
                       constant( q(i) )
,                       constant( q(e) )
                   )
                 )
               ] )
             ] )
           ] )
         )
,       concat( 
           capture( 'p6ws', 
             sub{ 
                 $grammar->p6ws( @_ );
             }
           )
,         concat( 
             capture( 'list', 
               sub{ 
                   $grammar->list( @_ );
               }
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 constant( q!;! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { _print =>  $_[0]->()  ,} } };
                     }
                 )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_print'} = ${'_print'}->code();
${'_my'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(m) )
,                 constant( q(y) )
             )
,             alternation( [
               concat( 
                   constant( q(o) )
,                 concat( 
                     constant( q(u) )
,                     constant( q(r) )
                 )
               )
,               concat( 
                   constant( q(l) )
,                 concat( 
                     constant( q(o) )
,                   concat( 
                       constant( q(c) )
,                     concat( 
                         constant( q(a) )
,                         constant( q(l) )
                     )
                   )
                 )
               )
             ] )
           ] )
         )
,       concat( 
           capture( 'p6ws', 
             sub{ 
                 $grammar->p6ws( @_ );
             }
           )
,         concat( 
             capture( 'variable', 
               sub{ 
                   $grammar->variable( @_ );
               }
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 constant( q!;! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { _my =>  $_[0]->()  ,} } };
                     }
                 )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_my'} = ${'_my'}->code();
${'_simple_statement'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'op', 
           alternation( [
             concat( 
                 constant( q(d) )
,               concat( 
                   constant( q(i) )
,                   constant( q(e) )
               )
             )
,             concat( 
                 constant( q!.! )
,               concat( 
                   constant( q!.! )
,                   constant( q!.! )
               )
             )
           ] )
         )
,       concat( 
           constant( q!;! )
,           abort(
               sub {
                   return { bool => 1, tail => $_[0], return => sub { return { _simple_statement =>  $_[0]->()  ,} } };
               }
           )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_simple_statement'} = ${'_simple_statement'}->code();
${'sub_decl'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(s) )
,       concat( 
           constant( q(u) )
,         concat( 
             constant( q(b) )
,           concat( 
               sub{ 
                   $grammar->p6ws( @_ );
               }
,             concat( 
                 capture( 'fix', 
                   alternation( [
                     concat( 
                         constant( q(i) )
,                       concat( 
                           constant( q(n) )
,                         concat( 
                             constant( q(f) )
,                           concat( 
                               constant( q(i) )
,                               constant( q(x) )
                           )
                         )
                       )
                     )
,                     alternation( [
                       concat( 
                           constant( q(p) )
,                         concat( 
                             constant( q(r) )
,                           concat( 
                               constant( q(e) )
,                             concat( 
                                 constant( q(f) )
,                               concat( 
                                   constant( q(i) )
,                                   constant( q(x) )
                               )
                             )
                           )
                         )
                       )
,                       concat( 
                           constant( q(p) )
,                         concat( 
                             constant( q(o) )
,                           concat( 
                               constant( q(s) )
,                             concat( 
                                 constant( q(t) )
,                               concat( 
                                   constant( q(f) )
,                                 concat( 
                                     constant( q(i) )
,                                     constant( q(x) )
                                 )
                               )
                             )
                           )
                         )
                       )
                     ] )
                   ] )
                 )
,               concat( 
                   constant( q!:! )
,                 concat( 
                     constant( q!<! )
,                   concat( 
                       capture( 'id', 
                         capture( '*quantifier*',
                             non_greedy_star(
                                   sub{ 
                                       $grammar->any( @_ );
                                   }
                             )
                         )
                       )
,                     concat( 
                         constant( q!>! )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( @_ );
                                   }
                             )
                         )
,                         concat( 
                             capture( 'block', 
                               sub{ 
                                   $grammar->block( @_ );
                               }
                             )
,                             abort(
                                 sub {
                                     return { bool => 1, tail => $_[0], return => sub { return { sub_decl =>  $_[0]->()  ,} } };
                                 }
                             )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_decl'} = ${'sub_decl'}->code();
${'sub_defin'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(s) )
,       concat( 
           constant( q(u) )
,         concat( 
             constant( q(b) )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( @_ );
                       }
                 )
             )
,             concat( 
                 capture( 'ident', 
                   sub{ 
                       $grammar->ident( @_ );
                   }
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                     )
                 )
,                 concat( 
                     capture( 'block', 
                       sub{ 
                           $grammar->block( @_ );
                       }
                     )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { sub_defin =>  $_[0]->()  ,} } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_defin'} = ${'sub_defin'}->code();
${'term2'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'term1', 
             capture( 'term1', 
               sub{ 
                   $grammar->term1( @_ );
               }
             )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   capture( 'p6ws', 
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
                   )
             )
         )
,         concat( 
             capture( 'op', 
                 alternation( \@Grammar::Perl6::ops )
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 capture( 'term2', 
                     capture( 'term1', 
                       sub{ 
                           $grammar->term1( @_ );
                       }
                     )
                 )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { sub_application_term =>  $_[0]->()  ,} } };
                     }
                 )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'term2'} = ${'term2'}->code();
${'sub_application'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         capture( 'term1', 
           alternation( [
               capture( 'term1', 
                 sub{ 
                     $grammar->term1( @_ );
                 }
               )
,               capture( 'term2', 
                 sub{ 
                     $grammar->term2( @_ );
                 }
               )
           ] )
         )
,       concat( 
         capture( '*quantifier*',
             optional(
                   capture( 'p6ws', 
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
                   )
             )
         )
,         concat( 
             capture( 'op', 
                 alternation( \@Grammar::Perl6::ops )
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       capture( 'p6ws', 
                         sub{ 
                             $grammar->p6ws( @_ );
                         }
                       )
                 )
             )
,             concat( 
                 capture( 'term2', 
                   alternation( [
                       capture( 'term1', 
                         sub{ 
                             $grammar->term1( @_ );
                         }
                       )
,                       capture( 'term2', 
                         sub{ 
                             $grammar->term2( @_ );
                         }
                       )
                   ] )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           capture( 'p6ws', 
                             sub{ 
                                 $grammar->p6ws( @_ );
                             }
                           )
                     )
                 )
,                 concat( 
                     constant( q!;! )
,                     abort(
                         sub {
                             return { bool => 1, tail => $_[0], return => sub { return { sub_application =>  $_[0]->()  ,} } };
                         }
                     )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'sub_application'} = ${'sub_application'}->code();
${'eval_perl5'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(e) )
,       concat( 
           constant( q(v) )
,         concat( 
             constant( q(a) )
,           concat( 
               constant( q(l) )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( @_ );
                           }
                         )
                   )
               )
,               concat( 
                   constant( q!(! )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             capture( 'p6ws', 
                               sub{ 
                                   $grammar->p6ws( @_ );
                               }
                             )
                       )
                   )
,                   concat( 
                       capture( 'literal', 
                         sub{ 
                             $grammar->literal( @_ );
                         }
                       )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 capture( 'p6ws', 
                                   sub{ 
                                       $grammar->p6ws( @_ );
                                   }
                                 )
                           )
                       )
,                       concat( 
                           constant( q!,! )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     capture( 'p6ws', 
                                       sub{ 
                                           $grammar->p6ws( @_ );
                                       }
                                     )
                               )
                           )
,                           concat( 
                               constant( q!:! )
,                             concat( 
                                 constant( q(l) )
,                               concat( 
                                   constant( q(a) )
,                                 concat( 
                                     constant( q(n) )
,                                   concat( 
                                       constant( q(g) )
,                                     concat( 
                                         constant( q!<! )
,                                       concat( 
                                           constant( q(p) )
,                                         concat( 
                                             constant( q(e) )
,                                           concat( 
                                               constant( q(r) )
,                                             concat( 
                                                 constant( q(l) )
,                                               concat( 
                                                   constant( q(5) )
,                                                 concat( 
                                                     constant( q!>! )
,                                                   concat( 
                                                     capture( '*quantifier*',
                                                         optional(
                                                               capture( 'p6ws', 
                                                                 sub{ 
                                                                     $grammar->p6ws( @_ );
                                                                 }
                                                               )
                                                         )
                                                     )
,                                                     concat( 
                                                         constant( q!)! )
,                                                       concat( 
                                                         capture( '*quantifier*',
                                                             optional(
                                                                   capture( 'p6ws', 
                                                                     sub{ 
                                                                         $grammar->p6ws( @_ );
                                                                     }
                                                                   )
                                                             )
                                                         )
,                                                         concat( 
                                                             constant( q!;! )
,                                                             abort(
                                                                 sub {
                                                                     return { bool => 1, tail => $_[0], return => sub { return { eval_perl5 =>  $_[0]->{literal}  } } };
                                                                 }
                                                             )
                                                         )
                                                       )
                                                     )
                                                   )
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'eval_perl5'} = ${'eval_perl5'}->code();
${'_return'} = Runtime::RuleCompiler->compiled(
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
     concat( 
         constant( q(r) )
,       concat( 
           constant( q(e) )
,         concat( 
             constant( q(t) )
,           concat( 
               constant( q(u) )
,             concat( 
                 constant( q(r) )
,               concat( 
                   constant( q(n) )
,                 concat( 
                     sub{ 
                         $grammar->p6ws( @_ );
                     }
,                   concat( 
                       capture( 'val', 
                         alternation( [
                             capture( 'term1', 
                               sub{ 
                                   $grammar->term1( @_ );
                               }
                             )
,                             capture( 'term2', 
                               sub{ 
                                   $grammar->term2( @_ );
                               }
                             )
                         ] )
                       )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( @_ );
                                 }
                           )
                       )
,                       concat( 
                           constant( q!;! )
,                           abort(
                               sub {
                                   return { bool => 1, tail => $_[0], return => sub { return { _return =>  $_[0]->()  ,} } };
                               }
                           )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
        ->( $_[0], undef, $tree, $tree )
    );
}

);
*{'_return'} = ${'_return'}->code();

push @terms, sub { Grammar::Perl6->indirect_object(@_) };
push @statements, sub { Grammar::Perl6->rule_decl(@_) };
push @statements, sub { Grammar::Perl6->grammar_name(@_) };
push @statements, sub { Grammar::Perl6->condition_rule(@_) };
push @statements, sub { Grammar::Perl6->meth_call_statement(@_) };
push @terms, sub { Grammar::Perl6->meth_call_term(@_) };
push @statements, sub { Grammar::Perl6->sub_call_statement(@_) };
push @terms, sub { Grammar::Perl6->sub_call_term(@_) };
push @terms, sub { Grammar::Perl6->access_hashref_element(@_) };
push @statements, sub { Grammar::Perl6->access_hashref_element(@_) };
push @terms, sub { Grammar::Perl6->access_hash_element(@_) };
push @statements, sub { Grammar::Perl6->access_hash_element(@_) };
push @statements, sub { Grammar::Perl6->assign_hash_to_scalar(@_) };
push @statements, sub { Grammar::Perl6->assign_slurp_to_variable(@_) };
push @statements, sub { Grammar::Perl6->assign_open_to_variable(@_) };
push @statements, sub { Grammar::Perl6->assign(@_) };
push @statements, sub { Grammar::Perl6->sub_call(@_) };
push @terms, sub { Grammar::Perl6->sub_call(@_) };
push @statements, sub { Grammar::Perl6->_push(@_) };
push @statements, sub { Grammar::Perl6->pod(@_) };
push @statements, sub { Grammar::Perl6->use_v6(@_) };
push @statements, sub { Grammar::Perl6->require(@_) };
push @statements, sub { Grammar::Perl6->use_rule(@_) };
push @statements, sub { Grammar::Perl6->block(@_) };
push @statements, sub { Grammar::Perl6->macro_decl(@_) };
push @terms, sub { Grammar::Perl6->empty_list(@_) };
push @terms, sub { Grammar::Perl6->varhash(@_) };
push @terms, sub { Grammar::Perl6->varscalar(@_) };
push @terms, sub { Grammar::Perl6->variable(@_) };
push @terms, sub { Grammar::Perl6->literal(@_) };
push @statements, sub { Grammar::Perl6->_open(@_) };
push @terms, sub { Grammar::Perl6->_open(@_) };
push @statements, sub { Grammar::Perl6->_print_with_fh(@_) };
push @statements, sub { Grammar::Perl6->_print(@_) };
push @statements, sub { Grammar::Perl6->_my(@_) };
push @statements, sub { Grammar::Perl6->_simple_statement(@_) };
push @statements, sub { Grammar::Perl6->sub_decl(@_) };
push @statements, sub { Grammar::Perl6->sub_defin(@_) };
push @statements, sub { Grammar::Perl6->sub_application(@_) };
push @statements, sub { Grammar::Perl6->eval_perl5(@_) };
push @statements, sub { Grammar::Perl6->_return(@_) };

1;
