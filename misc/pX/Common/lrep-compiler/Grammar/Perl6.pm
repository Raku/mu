package Grammar::Perl6;
use base 'Pugs::Grammar::Base', 'Pugs::Grammar::Rule', 'Grammar::Perl6Init';
use Runtime::RuleCompiler qw(rule);
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
*{'immediate_statement_rule'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
         capture( '*quantifier*',
             optional(
                   sub{ 
                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                   }
             )
         )
,         concat( 
             alternation( \@Grammar::Perl6::statements )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                 )
             )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return  $_[0]->()  } };
                   }
               )
           )
         )
       )
};
*{'grammar'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
         capture( '*quantifier*',
             greedy_star(
                   capture( 'immediate_statement_rule', 
                     sub{ 
                         $grammar->immediate_statement_rule( $_[0], { p => 1 }, $_[1] );
                     }
                   )
             )
         )
,           abort(
               sub {
                   return { bool => 1, tail => $_[0], return => sub { return  $_[0]->()  } };
               }
           )
       )
};
*{'indirect_object'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'varscalar', 
             sub{ 
                 $grammar->varscalar( $_[0], { p => 1 }, $_[1] );
             }
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     capture( 'p6ws', 
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                     )
               )
           )
,           concat( 
               constant( q!:! )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return  $_[0]->{varscalar}  } };
                   }
               )
           )
         )
       )
};
*{'rule_decl'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!r! )
,         concat( 
             constant( q!u! )
,           concat( 
               constant( q!l! )
,             concat( 
                 constant( q!e! )
,               concat( 
                   capture( 'p6ws', 
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
                   )
,                 concat( 
                     capture( 'ident', 
                       sub{ 
                           $grammar->ident( $_[0], { p => 1 }, $_[1] );
                       }
                     )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               capture( 'p6ws', 
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                         )
                     )
,                     concat( 
                         constant( q!{! )
,                       concat( 
                           capture( 'rule', 
                             sub{ 
                                 $grammar->rule( $_[0], { p => 1 }, $_[1] );
                             }
                           )
,                         concat( 
                             constant( q!}! )
,                             abort(
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
};
*{'grammar_name'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!g! )
,         concat( 
             constant( q!r! )
,           concat( 
               constant( q!a! )
,             concat( 
                 constant( q!m! )
,               concat( 
                   constant( q!m! )
,                 concat( 
                     constant( q!a! )
,                   concat( 
                       constant( q!r! )
,                     concat( 
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
,                       concat( 
                           capture( 'ident', 
                             sub{ 
                                 $grammar->ident( $_[0], { p => 1 }, $_[1] );
                             }
                           )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     capture( 'p6ws', 
                                       sub{ 
                                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                       }
                                     )
                               )
                           )
,                           concat( 
                               constant( q!;! )
,                               abort(
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
};
*{'condition_rule'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!i! )
,                   constant( q!f! )
               )
,               concat( 
                   constant( q!u! )
,                 concat( 
                     constant( q!n! )
,                   concat( 
                       constant( q!l! )
,                     concat( 
                         constant( q!e! )
,                       concat( 
                           constant( q!s! )
,                           constant( q!s! )
                       )
                     )
                   )
                 )
               )
             ] )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!(! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'condition', 
                       capture( 'term1', 
                         sub{ 
                             $grammar->term1( $_[0], { p => 1 }, $_[1] );
                         }
                       )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!)! )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                           )
                       )
,                       concat( 
                           capture( 'then', 
                               capture( 'block', 
                                 sub{ 
                                     $grammar->block( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                           )
,                           abort(
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
};
*{'meth_call_term'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'class', 
               capture( 'ident', 
                 sub{ 
                     $grammar->ident( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
             constant( q!.! )
,           concat( 
               capture( 'meth', 
                   capture( 'word', 
                     sub{ 
                         $grammar->word( $_[0], { p => 1 }, $_[1] );
                     }
                   )
               )
,             concat( 
                 constant( q!(! )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                     )
                 )
,                 concat( 
                     capture( 'params', 
                       capture( '*quantifier*',
                           optional(
                                 capture( 'list', 
                                   sub{ 
                                       $grammar->list( $_[0], { p => 1 }, $_[1] );
                                   }
                                 )
                           )
                       )
                     )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                         )
                     )
,                     concat( 
                         constant( q!)! )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
,                           abort(
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
};
*{'meth_call_statement'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'class', 
               capture( 'ident', 
                 sub{ 
                     $grammar->ident( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
             constant( q!.! )
,           concat( 
               capture( 'meth', 
                   capture( 'word', 
                     sub{ 
                         $grammar->word( $_[0], { p => 1 }, $_[1] );
                     }
                   )
               )
,             concat( 
                 constant( q!(! )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                     )
                 )
,                 concat( 
                     capture( 'params', 
                       capture( '*quantifier*',
                           optional(
                                 capture( 'list', 
                                   sub{ 
                                       $grammar->list( $_[0], { p => 1 }, $_[1] );
                                   }
                                 )
                           )
                       )
                     )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                         )
                     )
,                     concat( 
                         constant( q!)! )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
,                         concat( 
                             constant( q!;! )
,                             abort(
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
};
*{'sub_call_term'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'name', 
               capture( 'ident', 
                 sub{ 
                     $grammar->ident( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
             constant( q!(! )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                 )
             )
,             concat( 
                 capture( 'params', 
                   capture( '*quantifier*',
                       optional(
                             capture( 'list', 
                               sub{ 
                                   $grammar->list( $_[0], { p => 1 }, $_[1] );
                               }
                             )
                       )
                   )
                 )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                     )
                 )
,                 concat( 
                     constant( q!)! )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                         )
                     )
,                       abort(
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
};
*{'sub_call_statement'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'name', 
               capture( 'ident', 
                 sub{ 
                     $grammar->ident( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!(! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'params', 
                     capture( '*quantifier*',
                         optional(
                               capture( 'list', 
                                 sub{ 
                                     $grammar->list( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                         )
                     )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!)! )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                           )
                       )
,                       concat( 
                           constant( q!;! )
,                           abort(
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
};
*{'access_hashref_element'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'varscalar', 
                 sub{ 
                     $grammar->varscalar( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
             constant( q!{! )
,           concat( 
               capture( 'key', 
                   capture( 'term1', 
                     sub{ 
                         $grammar->term1( $_[0], { p => 1 }, $_[1] );
                     }
                   )
               )
,             concat( 
                 constant( q!}! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { access_hashref_element =>  $_[0]->()  } } };
                     }
                 )
             )
           )
         )
       )
};
*{'access_hash_element'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'varhash', 
                 sub{ 
                     $grammar->varhash( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
             constant( q!{! )
,           concat( 
               capture( 'key', 
                   capture( 'term1', 
                     sub{ 
                         $grammar->term1( $_[0], { p => 1 }, $_[1] );
                     }
                   )
               )
,             concat( 
                 constant( q!}! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { access_hash_element =>  $_[0]->()  } } };
                     }
                 )
             )
           )
         )
       )
};
*{'assign_hash_to_scalar'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'varscalar', 
                 sub{ 
                     $grammar->varscalar( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!=! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'value', 
                       capture( 'varhash', 
                         sub{ 
                             $grammar->varhash( $_[0], { p => 1 }, $_[1] );
                         }
                       )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!;! )
,                       abort(
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
};
*{'assign_slurp_to_variable'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'variable', 
                 sub{ 
                     $grammar->variable( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!=! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   constant( q!s! )
,                 concat( 
                     constant( q!l! )
,                   concat( 
                       constant( q!u! )
,                     concat( 
                         constant( q!r! )
,                       concat( 
                           constant( q!p! )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     sub{ 
                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                     }
                               )
                           )
,                           concat( 
                               capture( 'value', 
                                   capture( 'term1', 
                                     sub{ 
                                         $grammar->term1( $_[0], { p => 1 }, $_[1] );
                                     }
                                   )
                               )
,                             concat( 
                               capture( '*quantifier*',
                                   optional(
                                         sub{ 
                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                         }
                                   )
                               )
,                               concat( 
                                   constant( q!;! )
,                                   abort(
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
};
*{'assign_open_to_variable'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'variable', 
                 sub{ 
                     $grammar->variable( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!=! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   constant( q!o! )
,                 concat( 
                     constant( q!p! )
,                   concat( 
                       constant( q!e! )
,                     concat( 
                         constant( q!n! )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
,                         concat( 
                             capture( 'value', 
                                 capture( 'term1', 
                                   sub{ 
                                       $grammar->term1( $_[0], { p => 1 }, $_[1] );
                                   }
                                 )
                             )
,                           concat( 
                             capture( '*quantifier*',
                                 optional(
                                       sub{ 
                                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                       }
                                 )
                             )
,                             concat( 
                                 constant( q!;! )
,                                 abort(
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
};
*{'assign'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'variable', 
               capture( 'term1', 
                 sub{ 
                     $grammar->term1( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!=! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'value', 
                       capture( 'term1', 
                         sub{ 
                             $grammar->term1( $_[0], { p => 1 }, $_[1] );
                         }
                       )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!;! )
,                       abort(
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
};
*{'sub_call'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'name', 
               capture( 'ident', 
                 sub{ 
                     $grammar->ident( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
               )
           )
,           concat( 
               constant( q!(! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'params', 
                     capture( '*quantifier*',
                         optional(
                               capture( 'list', 
                                 sub{ 
                                     $grammar->list( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                         )
                     )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!)! )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                           )
                       )
,                       concat( 
                           constant( q!;! )
,                           abort(
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
};
*{'_push'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!p! )
,                 concat( 
                     constant( q!u! )
,                   concat( 
                       constant( q!s! )
,                       constant( q!h! )
                   )
                 )
               )
,               concat( 
                   constant( q!u! )
,                 concat( 
                     constant( q!n! )
,                   concat( 
                       constant( q!s! )
,                     concat( 
                         constant( q!h! )
,                       concat( 
                           constant( q!i! )
,                         concat( 
                             constant( q!f! )
,                             constant( q!t! )
                         )
                       )
                     )
                   )
                 )
               )
             ] )
           )
,         concat( 
             capture( 'p6ws', 
               sub{ 
                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
               }
             )
,           concat( 
               capture( 'variable', 
                 sub{ 
                     $grammar->variable( $_[0], { p => 1 }, $_[1] );
                 }
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   constant( q!,! )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             capture( 'p6ws', 
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                             )
                       )
                   )
,                   concat( 
                       capture( 'code', 
                         capture( '*quantifier*',
                             non_greedy_star(
                                   sub{ 
                                       $grammar->any( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
                       )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 capture( 'p6ws', 
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                                 )
                           )
                       )
,                       concat( 
                           constant( q!;! )
,                           abort(
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
};
*{'pod'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!=! )
,         concat( 
             alternation( [
               concat( 
                   constant( q!p! )
,                 concat( 
                     constant( q!o! )
,                     constant( q!d! )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q!h! )
,                   concat( 
                       constant( q!e! )
,                     concat( 
                         constant( q!a! )
,                       concat( 
                           constant( q!d! )
,                           constant( q!1! )
                       )
                     )
                   )
                 )
,                 alternation( [
                   concat( 
                       constant( q!k! )
,                     concat( 
                         constant( q!w! )
,                       concat( 
                           constant( q!i! )
,                           constant( q!d! )
                       )
                     )
                   )
,                   concat( 
                       constant( q!f! )
,                     concat( 
                         constant( q!o! )
,                         constant( q!r! )
                     )
                   )
                 ] )
               ] )
             ] )
,           concat( 
             capture( '*quantifier*',
                 non_greedy_star(
                       sub{ 
                           $grammar->any( $_[0], { p => 1 }, $_[1] );
                       }
                 )
             )
,             concat( 
                 constant( q!=! )
,               concat( 
                   constant( q!c! )
,                 concat( 
                     constant( q!u! )
,                     constant( q!t! )
                 )
               )
             )
           )
         )
       )
};
*{'use_v6'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!u! )
,         concat( 
             constant( q!s! )
,           concat( 
               constant( q!e! )
,             concat( 
                 sub{ 
                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                 }
,               concat( 
                   constant( q!v! )
,                 concat( 
                     constant( q!6! )
,                   concat( 
                       constant( q!-! )
,                     concat( 
                         constant( q!p! )
,                       concat( 
                           constant( q!u! )
,                         concat( 
                             constant( q!g! )
,                           concat( 
                               constant( q!s! )
,                             concat( 
                               capture( '*quantifier*',
                                   optional(
                                         sub{ 
                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                         }
                                   )
                               )
,                                 constant( q!;! )
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
};
*{'require'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!r! )
,         concat( 
             constant( q!e! )
,           concat( 
               constant( q!q! )
,             concat( 
                 constant( q!u! )
,               concat( 
                   constant( q!i! )
,                 concat( 
                     constant( q!r! )
,                   concat( 
                       constant( q!e! )
,                     concat( 
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
,                       concat( 
                           capture( 'ident', 
                             sub{ 
                                 $grammar->ident( $_[0], { p => 1 }, $_[1] );
                             }
                           )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     sub{ 
                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                     }
                               )
                           )
,                           concat( 
                               constant( q!;! )
,                               abort(
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
};
*{'use_rule'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!u! )
,         concat( 
             constant( q!s! )
,           concat( 
               constant( q!e! )
,             concat( 
                 sub{ 
                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                 }
,               concat( 
                   capture( 'ident', 
                     sub{ 
                         $grammar->ident( $_[0], { p => 1 }, $_[1] );
                     }
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       constant( q!;! )
,                       abort(
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
};
*{'term1'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
             alternation( \@Grammar::Perl6::terms )
};
*{'list'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
         capture( '*quantifier*',
             greedy_star(
                   concat( 
                       capture( 'term1', 
                         sub{ 
                             $grammar->term1( $_[0], { p => 1 }, $_[1] );
                         }
                       )
,                     concat( 
                       capture( '*quantifier*',
                           optional(
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                           )
                       )
,                       concat( 
                           constant( q!,! )
,                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
                       )
                     )
                   )
             )
         )
,         capture( '*quantifier*',
             optional(
                   capture( 'term1', 
                     sub{ 
                         $grammar->term1( $_[0], { p => 1 }, $_[1] );
                     }
                   )
             )
         )
       )
};
*{'block'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!{! )
,         concat( 
             capture( 'list', 
               capture( '*quantifier*',
                   greedy_star(
                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     sub{ 
                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                     }
                               )
                           )
,                             alternation( \@Grammar::Perl6::statements )
                         )
                   )
               )
             )
,           concat( 
             capture( '*quantifier*',
                 optional(
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                 )
             )
,             concat( 
                 constant( q!}! )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { block =>  $_[0]->{list}  ,} } };
                     }
                 )
             )
           )
         )
       )
};
*{'macro_decl'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!m! )
,         concat( 
             constant( q!a! )
,           concat( 
               constant( q!c! )
,             concat( 
                 constant( q!r! )
,               concat( 
                   constant( q!o! )
,                 concat( 
                     sub{ 
                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                     }
,                   concat( 
                       capture( 'prefix', 
                           capture( 'word', 
                             sub{ 
                                 $grammar->word( $_[0], { p => 1 }, $_[1] );
                             }
                           )
                       )
,                     concat( 
                         constant( q!:! )
,                       concat( 
                           constant( q!<! )
,                         concat( 
                             capture( 'id', 
                               capture( '*quantifier*',
                                   non_greedy_star(
                                         sub{ 
                                             $grammar->any( $_[0], { p => 1 }, $_[1] );
                                         }
                                   )
                               )
                             )
,                           concat( 
                               constant( q!>! )
,                             concat( 
                               capture( '*quantifier*',
                                   optional(
                                         sub{ 
                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                         }
                                   )
                               )
,                               concat( 
                                   constant( q!(! )
,                                 concat( 
                                   capture( '*quantifier*',
                                       optional(
                                             sub{ 
                                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                             }
                                       )
                                   )
,                                   concat( 
                                     capture( '*quantifier*',
                                         optional(
                                               capture( 'list', 
                                                 sub{ 
                                                     $grammar->list( $_[0], { p => 1 }, $_[1] );
                                                 }
                                               )
                                         )
                                     )
,                                     concat( 
                                       capture( '*quantifier*',
                                           optional(
                                                 sub{ 
                                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                 }
                                           )
                                       )
,                                       concat( 
                                           constant( q!)! )
,                                         concat( 
                                           capture( '*quantifier*',
                                               optional(
                                                     sub{ 
                                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                     }
                                               )
                                           )
,                                           concat( 
                                               constant( q!i! )
,                                             concat( 
                                                 constant( q!s! )
,                                               concat( 
                                                   sub{ 
                                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                   }
,                                                 concat( 
                                                     constant( q!p! )
,                                                   concat( 
                                                       constant( q!a! )
,                                                     concat( 
                                                         constant( q!r! )
,                                                       concat( 
                                                           constant( q!s! )
,                                                         concat( 
                                                             constant( q!e! )
,                                                           concat( 
                                                               constant( q!d! )
,                                                             concat( 
                                                               capture( '*quantifier*',
                                                                   optional(
                                                                         sub{ 
                                                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                         }
                                                                   )
                                                               )
,                                                               concat( 
                                                                   constant( q!(! )
,                                                                 concat( 
                                                                   capture( '*quantifier*',
                                                                       optional(
                                                                             sub{ 
                                                                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                             }
                                                                       )
                                                                   )
,                                                                   concat( 
                                                                       constant( q!/! )
,                                                                     concat( 
                                                                       capture( '*quantifier*',
                                                                           optional(
                                                                                 sub{ 
                                                                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                                 }
                                                                           )
                                                                       )
,                                                                       concat( 
                                                                           capture( 'rule', 
                                                                             sub{ 
                                                                                 $grammar->rule( $_[0], { p => 1 }, $_[1] );
                                                                             }
                                                                           )
,                                                                         concat( 
                                                                           capture( '*quantifier*',
                                                                               optional(
                                                                                     sub{ 
                                                                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                                     }
                                                                               )
                                                                           )
,                                                                           concat( 
                                                                               constant( q!/! )
,                                                                             concat( 
                                                                               capture( '*quantifier*',
                                                                                   optional(
                                                                                         sub{ 
                                                                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                                         }
                                                                                   )
                                                                               )
,                                                                               concat( 
                                                                                   constant( q!)! )
,                                                                                 concat( 
                                                                                   capture( '*quantifier*',
                                                                                       optional(
                                                                                             sub{ 
                                                                                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                                             }
                                                                                       )
                                                                                   )
,                                                                                   concat( 
                                                                                       capture( 'code', 
                                                                                         sub{ 
                                                                                             $grammar->code( $_[0], { p => 1 }, $_[1] );
                                                                                         }
                                                                                       )
,                                                                                       abort(
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
};
*{'empty_list'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!(! )
,         concat( 
             constant( q!)! )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return { empty_list =>  $_[0]->()  } } };
                 }
             )
         )
       )
};
*{'_open'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             concat( 
                 constant( q!o! )
,               concat( 
                   constant( q!p! )
,                 concat( 
                     constant( q!e! )
,                     constant( q!n! )
                 )
               )
             )
           )
,         concat( 
             capture( 'p6ws', 
               sub{ 
                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
               }
             )
,           concat( 
               capture( 'varscalar', 
                 sub{ 
                     $grammar->varscalar( $_[0], { p => 1 }, $_[1] );
                 }
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   constant( q!;! )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { _open =>  $_[0]->() , } } };
                       }
                   )
               )
             )
           )
         )
       )
};
*{'_print_with_fh'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!p! )
,                 concat( 
                     constant( q!r! )
,                   concat( 
                       constant( q!i! )
,                     concat( 
                         constant( q!n! )
,                         constant( q!t! )
                     )
                   )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q!s! )
,                   concat( 
                       constant( q!a! )
,                       constant( q!y! )
                   )
                 )
,                 alternation( [
                   concat( 
                       constant( q!w! )
,                     concat( 
                         constant( q!a! )
,                       concat( 
                           constant( q!r! )
,                           constant( q!n! )
                       )
                     )
                   )
,                   concat( 
                       constant( q!d! )
,                     concat( 
                         constant( q!i! )
,                         constant( q!e! )
                     )
                   )
                 ] )
               ] )
             ] )
           )
,         concat( 
             capture( 'p6ws', 
               sub{ 
                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
               }
             )
,           concat( 
               capture( 'indirect_object', 
                 sub{ 
                     $grammar->indirect_object( $_[0], { p => 1 }, $_[1] );
                 }
               )
,             concat( 
                 capture( 'p6ws', 
                   sub{ 
                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                   }
                 )
,               concat( 
                   capture( 'list', 
                     sub{ 
                         $grammar->list( $_[0], { p => 1 }, $_[1] );
                     }
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             capture( 'p6ws', 
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                             )
                       )
                   )
,                   concat( 
                       constant( q!;! )
,                       abort(
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
};
*{'_print'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!p! )
,                 concat( 
                     constant( q!r! )
,                   concat( 
                       constant( q!i! )
,                     concat( 
                         constant( q!n! )
,                         constant( q!t! )
                     )
                   )
                 )
               )
,               alternation( [
                 concat( 
                     constant( q!s! )
,                   concat( 
                       constant( q!a! )
,                       constant( q!y! )
                   )
                 )
,                 alternation( [
                   concat( 
                       constant( q!w! )
,                     concat( 
                         constant( q!a! )
,                       concat( 
                           constant( q!r! )
,                           constant( q!n! )
                       )
                     )
                   )
,                   concat( 
                       constant( q!d! )
,                     concat( 
                         constant( q!i! )
,                         constant( q!e! )
                     )
                   )
                 ] )
               ] )
             ] )
           )
,         concat( 
             capture( 'p6ws', 
               sub{ 
                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
               }
             )
,           concat( 
               capture( 'list', 
                 sub{ 
                     $grammar->list( $_[0], { p => 1 }, $_[1] );
                 }
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   constant( q!;! )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { _print =>  $_[0]->()  ,} } };
                       }
                   )
               )
             )
           )
         )
       )
};
*{'_my'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!m! )
,                   constant( q!y! )
               )
,               alternation( [
                 concat( 
                     constant( q!o! )
,                   concat( 
                       constant( q!u! )
,                       constant( q!r! )
                   )
                 )
,                 concat( 
                     constant( q!l! )
,                   concat( 
                       constant( q!o! )
,                     concat( 
                         constant( q!c! )
,                       concat( 
                           constant( q!a! )
,                           constant( q!l! )
                       )
                     )
                   )
                 )
               ] )
             ] )
           )
,         concat( 
             capture( 'p6ws', 
               sub{ 
                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
               }
             )
,           concat( 
               capture( 'variable', 
                 sub{ 
                     $grammar->variable( $_[0], { p => 1 }, $_[1] );
                 }
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   constant( q!;! )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { _my =>  $_[0]->()  ,} } };
                       }
                   )
               )
             )
           )
         )
       )
};
*{'_simple_statement'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'op', 
             alternation( [
               concat( 
                   constant( q!d! )
,                 concat( 
                     constant( q!i! )
,                     constant( q!e! )
                 )
               )
,               concat( 
                   constant( q!.! )
,                 concat( 
                     constant( q!.! )
,                     constant( q!.! )
                 )
               )
             ] )
           )
,         concat( 
             constant( q!;! )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return { _simple_statement =>  $_[0]->()  ,} } };
                 }
             )
         )
       )
};
*{'sub_decl'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!s! )
,         concat( 
             constant( q!u! )
,           concat( 
               constant( q!b! )
,             concat( 
                 sub{ 
                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                 }
,               concat( 
                   capture( 'fix', 
                     alternation( [
                       concat( 
                           constant( q!i! )
,                         concat( 
                             constant( q!n! )
,                           concat( 
                               constant( q!f! )
,                             concat( 
                                 constant( q!i! )
,                                 constant( q!x! )
                             )
                           )
                         )
                       )
,                       alternation( [
                         concat( 
                             constant( q!p! )
,                           concat( 
                               constant( q!r! )
,                             concat( 
                                 constant( q!e! )
,                               concat( 
                                   constant( q!f! )
,                                 concat( 
                                     constant( q!i! )
,                                     constant( q!x! )
                                 )
                               )
                             )
                           )
                         )
,                         concat( 
                             constant( q!p! )
,                           concat( 
                               constant( q!o! )
,                             concat( 
                                 constant( q!s! )
,                               concat( 
                                   constant( q!t! )
,                                 concat( 
                                     constant( q!f! )
,                                   concat( 
                                       constant( q!i! )
,                                       constant( q!x! )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       ] )
                     ] )
                   )
,                 concat( 
                     constant( q!:! )
,                   concat( 
                       constant( q!<! )
,                     concat( 
                         capture( 'id', 
                           capture( '*quantifier*',
                               non_greedy_star(
                                     sub{ 
                                         $grammar->any( $_[0], { p => 1 }, $_[1] );
                                     }
                               )
                           )
                         )
,                       concat( 
                           constant( q!>! )
,                         concat( 
                           capture( '*quantifier*',
                               optional(
                                     sub{ 
                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                     }
                               )
                           )
,                           concat( 
                               capture( 'block', 
                                 sub{ 
                                     $grammar->block( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
,                               abort(
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
};
*{'sub_defin'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!s! )
,         concat( 
             constant( q!u! )
,           concat( 
               constant( q!b! )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         sub{ 
                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                         }
                   )
               )
,               concat( 
                   capture( 'ident', 
                     sub{ 
                         $grammar->ident( $_[0], { p => 1 }, $_[1] );
                     }
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                       )
                   )
,                   concat( 
                       capture( 'block', 
                         sub{ 
                             $grammar->block( $_[0], { p => 1 }, $_[1] );
                         }
                       )
,                       abort(
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
};
*{'term2'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'term1', 
               capture( 'term1', 
                 sub{ 
                     $grammar->term1( $_[0], { p => 1 }, $_[1] );
                 }
               )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     capture( 'p6ws', 
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                     )
               )
           )
,           concat( 
               capture( 'op', 
                   alternation( \@Grammar::Perl6::ops )
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   capture( 'term2', 
                       capture( 'term1', 
                         sub{ 
                             $grammar->term1( $_[0], { p => 1 }, $_[1] );
                         }
                       )
                   )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { sub_application_term =>  $_[0]->()  ,} } };
                       }
                   )
               )
             )
           )
         )
       )
};
*{'sub_application'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           capture( 'term1', 
             alternation( [
                 capture( 'term1', 
                   sub{ 
                       $grammar->term1( $_[0], { p => 1 }, $_[1] );
                   }
                 )
,                 capture( 'term2', 
                   sub{ 
                       $grammar->term2( $_[0], { p => 1 }, $_[1] );
                   }
                 )
             ] )
           )
,         concat( 
           capture( '*quantifier*',
               optional(
                     capture( 'p6ws', 
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
                     )
               )
           )
,           concat( 
               capture( 'op', 
                   alternation( \@Grammar::Perl6::ops )
               )
,             concat( 
               capture( '*quantifier*',
                   optional(
                         capture( 'p6ws', 
                           sub{ 
                               $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                   )
               )
,               concat( 
                   capture( 'term2', 
                     alternation( [
                         capture( 'term1', 
                           sub{ 
                               $grammar->term1( $_[0], { p => 1 }, $_[1] );
                           }
                         )
,                         capture( 'term2', 
                           sub{ 
                               $grammar->term2( $_[0], { p => 1 }, $_[1] );
                           }
                         )
                     ] )
                   )
,                 concat( 
                   capture( '*quantifier*',
                       optional(
                             capture( 'p6ws', 
                               sub{ 
                                   $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                               }
                             )
                       )
                   )
,                   concat( 
                       constant( q!;! )
,                       abort(
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
};
*{'eval_perl5'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!e! )
,         concat( 
             constant( q!v! )
,           concat( 
               constant( q!a! )
,             concat( 
                 constant( q!l! )
,               concat( 
                 capture( '*quantifier*',
                     optional(
                           capture( 'p6ws', 
                             sub{ 
                                 $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                             }
                           )
                     )
                 )
,                 concat( 
                     constant( q!(! )
,                   concat( 
                     capture( '*quantifier*',
                         optional(
                               capture( 'p6ws', 
                                 sub{ 
                                     $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                         )
                     )
,                     concat( 
                         capture( 'literal', 
                           sub{ 
                               $grammar->literal( $_[0], { p => 1 }, $_[1] );
                           }
                         )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   capture( 'p6ws', 
                                     sub{ 
                                         $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                     }
                                   )
                             )
                         )
,                         concat( 
                             constant( q!,! )
,                           concat( 
                             capture( '*quantifier*',
                                 optional(
                                       capture( 'p6ws', 
                                         sub{ 
                                             $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                         }
                                       )
                                 )
                             )
,                             concat( 
                                 constant( q!:! )
,                               concat( 
                                   constant( q!l! )
,                                 concat( 
                                     constant( q!a! )
,                                   concat( 
                                       constant( q!n! )
,                                     concat( 
                                         constant( q!g! )
,                                       concat( 
                                           constant( q!<! )
,                                         concat( 
                                             constant( q!p! )
,                                           concat( 
                                               constant( q!e! )
,                                             concat( 
                                                 constant( q!r! )
,                                               concat( 
                                                   constant( q!l! )
,                                                 concat( 
                                                     constant( q!5! )
,                                                   concat( 
                                                       constant( q!>! )
,                                                     concat( 
                                                       capture( '*quantifier*',
                                                           optional(
                                                                 capture( 'p6ws', 
                                                                   sub{ 
                                                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                   }
                                                                 )
                                                           )
                                                       )
,                                                       concat( 
                                                           constant( q!)! )
,                                                         concat( 
                                                           capture( '*quantifier*',
                                                               optional(
                                                                     capture( 'p6ws', 
                                                                       sub{ 
                                                                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                                                       }
                                                                     )
                                                               )
                                                           )
,                                                           concat( 
                                                               constant( q!;! )
,                                                               abort(
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
};
*{'_return'} = rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
           concat( 
           constant( q!r! )
,         concat( 
             constant( q!e! )
,           concat( 
               constant( q!t! )
,             concat( 
                 constant( q!u! )
,               concat( 
                   constant( q!r! )
,                 concat( 
                     constant( q!n! )
,                   concat( 
                       sub{ 
                           $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                       }
,                     concat( 
                         capture( 'val', 
                           alternation( [
                               capture( 'term1', 
                                 sub{ 
                                     $grammar->term1( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
,                               capture( 'term2', 
                                 sub{ 
                                     $grammar->term2( $_[0], { p => 1 }, $_[1] );
                                 }
                               )
                           ] )
                         )
,                       concat( 
                         capture( '*quantifier*',
                             optional(
                                   sub{ 
                                       $grammar->p6ws( $_[0], { p => 1 }, $_[1] );
                                   }
                             )
                         )
,                         concat( 
                             constant( q!;! )
,                             abort(
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
};
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
