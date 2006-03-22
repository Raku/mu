package Grammar::Perl6;
use base 'Pugs::Grammar::Base', 'Pugs::Grammar::Rule', 'Grammar::Perl6Init';
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
*{'immediate_statement_rule'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
         optional(
           sub { $grammar->p6ws( @_ ) }
         )
,         concat( 
             alternation( \@Grammar::Perl6::statements )
,           optional(
             sub { $grammar->p6ws( @_ ) }
           )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'grammar'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       greedy_star(
         sub { $grammar->immediate_statement_rule( @_ ) }
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'indirect_object'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           sub {  $grammar->varscalar( @_ ) } 
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( ':' )
,               abort(
                   sub {
                       return { bool => 1, tail => $_[0], return => sub { return  $_[0]->[varscalar]  } };
                   }
               )
           )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @terms, sub { Grammar::Perl6->indirect_object(@_) };
*{'rule_decl'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "rule" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->ident( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( '{' )
,                 concat( 
                     sub {  $grammar->rule( @_ ) } 
,                   concat( 
                       constant( '}' )
,                       abort(
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->rule_decl(@_) };
*{'grammar_name'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "grammar" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->ident( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { grammar_name =>  $_[0]->()  ,} } };
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
;
    push @statements, sub { Grammar::Perl6->grammar_name(@_) };
*{'condition_rule'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "if" )
,           constant( "unless" )
       ] )
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '(' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'condition', 
         sub {  $grammar->term1( @_ ) } 
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ')' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           capture( 'then', 
         sub {  $grammar->block( @_ ) } 
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->condition_rule(@_) };
*{'meth_call_term'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'class', 
         sub {  $grammar->ident( @_ ) } 
           )
,         concat( 
             constant( '.' )
,           concat( 
               capture( 'meth', 
         sub {  $grammar->word( @_ ) } 
               )
,             concat( 
                 constant( '(' )
,               concat( 
                 optional(
                   sub {  $grammar->p6ws( @_ ) } 
                 )
,                 concat( 
                     capture( 'params', 
       optional(
         sub {  $grammar->list( @_ ) } 
       )
                     )
,                   concat( 
                     optional(
                       sub {  $grammar->p6ws( @_ ) } 
                     )
,                     concat( 
                         constant( ')' )
,                       concat( 
                         optional(
                           sub {  $grammar->p6ws( @_ ) } 
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'meth_call_statement'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'class', 
         sub {  $grammar->ident( @_ ) } 
           )
,         concat( 
             constant( '.' )
,           concat( 
               capture( 'meth', 
         sub {  $grammar->word( @_ ) } 
               )
,             concat( 
                 constant( '(' )
,               concat( 
                 optional(
                   sub {  $grammar->p6ws( @_ ) } 
                 )
,                 concat( 
                     capture( 'params', 
       optional(
         sub {  $grammar->list( @_ ) } 
       )
                     )
,                   concat( 
                     optional(
                       sub {  $grammar->p6ws( @_ ) } 
                     )
,                     concat( 
                         constant( ')' )
,                       concat( 
                         optional(
                           sub {  $grammar->p6ws( @_ ) } 
                         )
,                         concat( 
                             constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->meth_call_statement(@_) };
    push @terms, sub { Grammar::Perl6->meth_call_term(@_) };
*{'sub_call_term'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'name', 
         sub {  $grammar->ident( @_ ) } 
           )
,         concat( 
             constant( '(' )
,           concat( 
             optional(
               sub {  $grammar->p6ws( @_ ) } 
             )
,             concat( 
                 capture( 'params', 
       optional(
         sub {  $grammar->list( @_ ) } 
       )
                 )
,               concat( 
                 optional(
                   sub {  $grammar->p6ws( @_ ) } 
                 )
,                 concat( 
                     constant( ')' )
,                   concat( 
                     optional(
                       sub {  $grammar->p6ws( @_ ) } 
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'sub_call_statement'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'name', 
         sub {  $grammar->ident( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '(' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'params', 
       optional(
         sub {  $grammar->list( @_ ) } 
       )
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ')' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->sub_call_statement(@_) };
    push @terms, sub { Grammar::Perl6->sub_call_term(@_) };
*{'access_hashref_element'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->varscalar( @_ ) } 
           )
,         concat( 
             constant( '{' )
,           concat( 
               capture( 'key', 
         sub {  $grammar->term1( @_ ) } 
               )
,             concat( 
                 constant( '}' )
,                 abort(
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
;
    push @terms, sub { Grammar::Perl6->access_hashref_element(@_) };
    push @statements, sub { Grammar::Perl6->access_hashref_element(@_) };
*{'access_hash_element'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->varhash( @_ ) } 
           )
,         concat( 
             constant( '{' )
,           concat( 
               capture( 'key', 
         sub {  $grammar->term1( @_ ) } 
               )
,             concat( 
                 constant( '}' )
,                 abort(
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
;
    push @terms, sub { Grammar::Perl6->access_hash_element(@_) };
    push @statements, sub { Grammar::Perl6->access_hash_element(@_) };
*{'assign_hash_to_scalar'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->varscalar( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '=' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'value', 
         sub {  $grammar->varhash( @_ ) } 
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->assign_hash_to_scalar(@_) };
*{'assign_slurp_to_variable'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->variable( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '=' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( "slurp" )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       capture( 'value', 
         sub {  $grammar->term1( @_ ) } 
                       )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ';' )
,                           abort(
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->assign_slurp_to_variable(@_) };
*{'assign_open_to_variable'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->variable( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '=' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( "open" )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       capture( 'value', 
         sub {  $grammar->term1( @_ ) } 
                       )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ';' )
,                           abort(
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->assign_open_to_variable(@_) };
*{'assign'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'variable', 
         sub {  $grammar->term1( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '=' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'value', 
         sub {  $grammar->term1( @_ ) } 
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->assign(@_) };
*{'sub_call'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'name', 
         sub {  $grammar->ident( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '(' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'params', 
       optional(
         sub {  $grammar->list( @_ ) } 
       )
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ')' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->sub_call(@_) };
    push @terms, sub { Grammar::Perl6->sub_call(@_) };
*{'_push'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "push" )
,           constant( "unshift" )
       ] )
           )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->variable( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ',' )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       capture( 'code', 
       non_greedy_star(
         sub{ 
             ${ $grammar->any( @_ ) };
         }
       )
                       )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_push(@_) };
*{'pod'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( '=' )
,         concat( 
             alternation( [
                 constant( "pod" )
,               alternation( [
                   constant( "head1" )
,                 alternation( [
                     constant( "kwid" )
,                     constant( "for" )
                 ] )
               ] )
             ] )
,           concat( 
             non_greedy_star(
               sub{ 
                   ${ $grammar->any( @_ ) };
               }
             )
,             concat( 
                 constant( '=' )
,                 constant( "cut" )
             )
           )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->pod(@_) };
*{'use_v6'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "use" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               constant( "v6" )
,             concat( 
                 constant( '-' )
,               concat( 
                   constant( "pugs" )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                     constant( ';' )
                 )
               )
             )
           )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->use_v6(@_) };
*{'require'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "require" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->ident( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { 
		# XXX This is perl5 code
		# this is ugly
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->require(@_) };
*{'use_rule'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "use" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->ident( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { 
		# XXX This is perl5 code
		# this is ugly
		# eval 'use '. $_[0]->() ->[2]{ident}[0]{ident};
		return { use_bareword =>  $_[0]->()  ,} 
	} };
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
;
    push @statements, sub { Grammar::Perl6->use_rule(@_) };
*{'term1'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
         alternation( \@terms )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'list'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
         greedy_star(
           concat( 
               sub {  $grammar->term1( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ',' )
,                 optional(
                   sub {  $grammar->p6ws( @_ ) } 
                 )
               )
             )
           )
         )
,         optional(
           sub {  $grammar->term1( @_ ) } 
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'block'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( '{' )
,         concat( 
             capture( 'list', 
       greedy_star(
         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,             alternation( \@statements )
         )
       )
             )
,           concat( 
             optional(
               sub {  $grammar->p6ws( @_ ) } 
             )
,             concat( 
                 constant( '}' )
,                 abort(
                     sub {
                         return { bool => 1, tail => $_[0], return => sub { return { block =>  $_[0]->[list]  ,} } };
                     }
                 )
             )
           )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->block(@_) };
*{'macro_decl'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "macro" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               capture( 'prefix', 
         sub {  $grammar->word( @_ ) } 
               )
,             concat( 
                 constant( ':' )
,               concat( 
                   constant( '<' )
,                 concat( 
                     capture( 'id', 
       non_greedy_star(
         sub{ 
             ${ $grammar->any( @_ ) };
         }
       )
                     )
,                   concat( 
                       constant( '>' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( '(' )
,                         concat( 
                           optional(
                             sub {  $grammar->p6ws( @_ ) } 
                           )
,                           concat( 
                             optional(
                               sub {  $grammar->list( @_ ) } 
                             )
,                             concat( 
                               optional(
                                 sub {  $grammar->p6ws( @_ ) } 
                               )
,                               concat( 
                                   constant( ')' )
,                                 concat( 
                                   optional(
                                     sub {  $grammar->p6ws( @_ ) } 
                                   )
,                                   concat( 
                                       constant( "is" )
,                                     concat( 
                                         sub {  $grammar->p6ws( @_ ) } 
,                                       concat( 
                                           constant( "parsed" )
,                                         concat( 
                                           optional(
                                             sub {  $grammar->p6ws( @_ ) } 
                                           )
,                                           concat( 
                                               constant( '(' )
,                                             concat( 
                                               optional(
                                                 sub {  $grammar->p6ws( @_ ) } 
                                               )
,                                               concat( 
                                                   constant( '/' )
,                                                 concat( 
                                                   optional(
                                                     sub {  $grammar->p6ws( @_ ) } 
                                                   )
,                                                   concat( 
                                                       sub {  $grammar->rule( @_ ) } 
,                                                     concat( 
                                                       optional(
                                                         sub {  $grammar->p6ws( @_ ) } 
                                                       )
,                                                       concat( 
                                                           constant( '/' )
,                                                         concat( 
                                                           optional(
                                                             sub {  $grammar->p6ws( @_ ) } 
                                                           )
,                                                           concat( 
                                                               constant( ')' )
,                                                             concat( 
                                                               optional(
                                                                 sub {  $grammar->p6ws( @_ ) } 
                                                               )
,                                                               concat( 
                                                                   sub {  $grammar->code( @_ ) } 
,                                                                   abort(
                                                                       sub {
                                                                           return { bool => 1, tail => $_[0], return => sub {
	 # XXX This is perl5 code
	 # XXX This is ugly
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->macro_decl(@_) };
*{'empty_list'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( '(' )
,         concat( 
             constant( ')' )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return { empty_list =>  $_[0]->()  } } };
                 }
             )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @terms, sub { Grammar::Perl6->empty_list(@_) };
    push @terms, sub { Grammar::Perl6->varhash(@_) };
    push @terms, sub { Grammar::Perl6->varscalar(@_) };
    push @terms, sub { Grammar::Perl6->variable(@_) };
    push @terms, sub { Grammar::Perl6->literal(@_) };
*{'_open'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
         constant( "open" )
           )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->varscalar( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_open(@_) };
    push @terms, sub { Grammar::Perl6->_open(@_) };
*{'_print_with_fh'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "print" )
,         alternation( [
             constant( "say" )
,           alternation( [
               constant( "warn" )
,               constant( "die" )
           ] )
         ] )
       ] )
           )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->indirect_object( @_ ) } 
,             concat( 
                 sub {  $grammar->p6ws( @_ ) } 
,               concat( 
                   sub {  $grammar->list( @_ ) } 
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_print_with_fh(@_) };
*{'_print'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "print" )
,         alternation( [
             constant( "say" )
,           alternation( [
               constant( "warn" )
,               constant( "die" )
           ] )
         ] )
       ] )
           )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->list( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_print(@_) };
*{'_my'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "my" )
,         alternation( [
             constant( "our" )
,             constant( "local" )
         ] )
       ] )
           )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               sub {  $grammar->variable( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_my(@_) };
*{'_simple_statement'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'op', 
       alternation( [
           constant( "die" )
,         concat( 
             constant( '.' )
,           concat( 
               constant( '.' )
,               constant( '.' )
           )
         )
       ] )
           )
,         concat( 
             constant( ';' )
,             abort(
                 sub {
                     return { bool => 1, tail => $_[0], return => sub { return { _simple_statement =>  $_[0]->()  ,} } };
                 }
             )
         )
       )
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->_simple_statement(@_) };
*{'sub_decl'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "sub" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               capture( 'fix', 
       alternation( [
           constant( "infix" )
,         alternation( [
             constant( "prefix" )
,             constant( "postfix" )
         ] )
       ] )
               )
,             concat( 
                 constant( ':' )
,               concat( 
                   constant( '<' )
,                 concat( 
                     capture( 'id', 
       non_greedy_star(
         sub{ 
             ${ $grammar->any( @_ ) };
         }
       )
                     )
,                   concat( 
                       constant( '>' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           sub {  $grammar->block( @_ ) } 
,                           abort(
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->sub_decl(@_) };
*{'sub_defin'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "sub" )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               sub {  $grammar->ident( @_ ) } 
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   sub {  $grammar->block( @_ ) } 
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { sub_defin =>  $_[0]->()  ,} } };
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
;
    push @statements, sub { Grammar::Perl6->sub_defin(@_) };
*{'term2'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'term1', 
         sub {  $grammar->term1( @_ ) } 
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               capture( 'op', 
         alternation( \@ops )
               )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'term2', 
         sub {  $grammar->term1( @_ ) } 
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
*{'sub_application'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           capture( 'term1', 
       alternation( [
           sub {  $grammar->term1( @_ ) } 
,           sub {  $grammar->term2( @_ ) } 
       ] )
           )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               capture( 'op', 
         alternation( \@ops )
               )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   capture( 'term2', 
       alternation( [
           sub {  $grammar->term1( @_ ) } 
,           sub {  $grammar->term2( @_ ) } 
       ] )
                   )
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ';' )
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->sub_application(@_) };
*{'eval_perl5'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "eval" )
,         concat( 
           optional(
             sub {  $grammar->p6ws( @_ ) } 
           )
,           concat( 
               constant( '(' )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   sub {  $grammar->literal( @_ ) } 
,                 concat( 
                   optional(
                     sub {  $grammar->p6ws( @_ ) } 
                   )
,                   concat( 
                       constant( ',' )
,                     concat( 
                       optional(
                         sub {  $grammar->p6ws( @_ ) } 
                       )
,                       concat( 
                           constant( ':' )
,                         concat( 
                             constant( "lang" )
,                           concat( 
                               constant( '<' )
,                             concat( 
                                 constant( "perl5" )
,                               concat( 
                                   constant( '>' )
,                                 concat( 
                                   optional(
                                     sub {  $grammar->p6ws( @_ ) } 
                                   )
,                                   concat( 
                                       constant( ')' )
,                                     concat( 
                                       optional(
                                         sub {  $grammar->p6ws( @_ ) } 
                                       )
,                                       concat( 
                                           constant( ';' )
,                                           abort(
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
        ->( $_[0], undef, $tree, $tree )
    );
}
;
    push @statements, sub { Grammar::Perl6->eval_perl5(@_) };
*{'_return'} = 
sub {
    my $grammar = shift;
    package Pugs::Runtime::Rule;
    my $tree;
    rule_wrapper( $_[0], 
       concat( 
           constant( "return" )
,         concat( 
             sub {  $grammar->p6ws( @_ ) } 
,           concat( 
               capture( 'val', 
       alternation( [
           sub {  $grammar->term1( @_ ) } 
,           sub {  $grammar->term2( @_ ) } 
       ] )
               )
,             concat( 
               optional(
                 sub {  $grammar->p6ws( @_ ) } 
               )
,               concat( 
                   constant( ';' )
,                   abort(
                       sub {
                           return { bool => 1, tail => $_[0], return => sub { return { _return =>  $_[0]->()  ,} } };
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
;
    push @statements, sub { Grammar::Perl6->_return(@_) };
