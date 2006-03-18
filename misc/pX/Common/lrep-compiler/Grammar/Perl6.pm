package Grammar::Perl6;
*{'immediate_statement_rule'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::alternation( \@statements )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
       )
;
*{'grammar'} = 
         Runtime::Perl5::RuleOps::greedy_star(
             Runtime::Perl5::RuleOps::capture( 'immediate_statement_rule', \&{'Grammar::Perl6::immediate_statement_rule'} )
           ,
         )
       ,
;
*{'indirect_object'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'varscalar', \&{'Grammar::Perl6::varscalar'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return  match::get( $_[0], '$()<varscalar>' )  }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @terms, \&indirect_object;
*{'condition_rule'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "if" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "unless" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'condition', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'then', 
             Runtime::Perl5::RuleOps::capture( 'block', \&{'Grammar::Perl6::block'} )
           ,
         )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { condition =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&condition_rule;
*{'meth_call_term'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'class', 
             Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\." )
       ,
         Runtime::Perl5::RuleOps::capture( 'meth', 
             Runtime::Perl5::RuleOps::capture( 'word', \&{'Grammar::Perl6::word'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'params', 
             Runtime::Perl5::RuleOps::optional(
                 Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { meth_call_term =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
*{'meth_call_statement'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'class', 
             Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\." )
       ,
         Runtime::Perl5::RuleOps::capture( 'meth', 
             Runtime::Perl5::RuleOps::capture( 'word', \&{'Grammar::Perl6::word'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'params', 
             Runtime::Perl5::RuleOps::optional(
                 Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { meth_call =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&meth_call_statement;
    push @terms, \&meth_call_term;
*{'sub_call_term'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'name', 
             Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'params', 
             Runtime::Perl5::RuleOps::optional(
                 Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_call_term =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
*{'sub_call_statement'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'name', 
             Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'params', 
             Runtime::Perl5::RuleOps::optional(
                 Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_call =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&sub_call_statement;
    push @terms, \&sub_call_term;
*{'access_hashref_element'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'varscalar', \&{'Grammar::Perl6::varscalar'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'key', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { access_hashref_element =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @terms, \&access_hashref_element;
    push @statements, \&access_hashref_element;
*{'access_hash_element'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'varhash', \&{'Grammar::Perl6::varhash'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'key', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { access_hash_element =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @terms, \&access_hash_element;
    push @statements, \&access_hash_element;
*{'assign_hash_to_scalar'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'varscalar', \&{'Grammar::Perl6::varscalar'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'value', 
             Runtime::Perl5::RuleOps::capture( 'varhash', \&{'Grammar::Perl6::varhash'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { assign_hash_to_scalar =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&assign_hash_to_scalar;
*{'assign_slurp_to_variable'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "slurp" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'value', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { slurp =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&assign_slurp_to_variable;
*{'assign_open_to_variable'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "open" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'value', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _open =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&assign_open_to_variable;
*{'assign'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'variable', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'value', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { assign =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&assign;
*{'rule_decl'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "rule" )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'rule', \&{'Grammar::Perl6::rule'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { rule_decl =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&rule_decl;
*{'grammar_name'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "grammar" )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { grammar_name =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&grammar_name;
*{'sub_call'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'name', 
             Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'params', 
             Runtime::Perl5::RuleOps::optional(
                 Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_call =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&sub_call;
    push @terms, \&sub_call;
*{'_push'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "push" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "unshift" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\," )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'code', 
             Runtime::Perl5::RuleOps::non_greedy_star(
                 \&{'Grammar::Perl6::any'}
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _push =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_push;
*{'pod'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::alternation( [
               Runtime::Perl5::RuleOps::constant( "pod" )
             ,
               Runtime::Perl5::RuleOps::constant( "head1" )
             ,
               Runtime::Perl5::RuleOps::constant( "kwid" )
             ,
               Runtime::Perl5::RuleOps::constant( "for" )
             ,
         ] )
       ,
         Runtime::Perl5::RuleOps::non_greedy_star(
             \&{'Grammar::Perl6::any'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::constant( "cut" )
       ,
       )
;
    push @statements, \&pod;
*{'use_v6'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "use" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::constant( "v6" )
       ,
         Runtime::Perl5::RuleOps::constant( "\-" )
       ,
         Runtime::Perl5::RuleOps::constant( "pugs" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
;
    push @statements, \&use_v6;
*{'require'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "require" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { 
		# XXX This is perl5 code
		# this is ugly
		eval 'require '. match::get( $_[0], '$()' ) ->[2]{ident}[0]{ident};
		return { require_bareword =>  match::get( $_[0], '$()' )  ,} 
	}       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&require;
*{'use_rule'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "use" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { 
		# XXX This is perl5 code
		# this is ugly
		eval 'use '. match::get( $_[0], '$()' ) ->[2]{ident}[0]{ident};
		return { use_bareword =>  match::get( $_[0], '$()' )  ,} 
	}       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&use_rule;
*{'term1'} = 
         Runtime::Perl5::RuleOps::alternation( \@terms )
       ,
;
*{'list'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::greedy_star(
           Runtime::Perl5::RuleOps::concat(
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
             Runtime::Perl5::RuleOps::optional(
                 \&{'Grammar::Perl6::p6ws'}
               ,
             )
           ,
             Runtime::Perl5::RuleOps::constant( "\," )
           ,
             Runtime::Perl5::RuleOps::optional(
                 \&{'Grammar::Perl6::p6ws'}
               ,
             )
           ,
           )
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
       )
;
*{'block'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'list', 
             Runtime::Perl5::RuleOps::greedy_star(
               Runtime::Perl5::RuleOps::concat(
                 Runtime::Perl5::RuleOps::optional(
                     \&{'Grammar::Perl6::p6ws'}
                   ,
                 )
               ,
                 Runtime::Perl5::RuleOps::alternation( \@statements )
               ,
               )
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { block =>  match::get( $_[0], '$()<list>' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&block;
*{'macro_decl'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "macro" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::capture( 'prefix', 
             Runtime::Perl5::RuleOps::capture( 'word', \&{'Grammar::Perl6::word'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
         Runtime::Perl5::RuleOps::constant( "\<" )
       ,
         Runtime::Perl5::RuleOps::capture( 'id', 
             Runtime::Perl5::RuleOps::non_greedy_star(
                 \&{'Grammar::Perl6::any'}
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\>" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "is" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::constant( "parsed" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\/" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'rule', \&{'Grammar::Perl6::rule'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\/" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'code', \&{'Grammar::Perl6::code'} )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub {
	 # XXX This is perl5 code
	 # XXX This is ugly
	 eval Emitter::Perl5::emit({macro =>  match::get( $_[0], '$()' ) });
	 return { macro =>  match::get( $_[0], '$()' )  ,}
	}       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&macro_decl;
*{'empty_list'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { empty_list =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @terms, \&empty_list;
    push @terms, \&varhash;
    push @terms, \&varscalar;
    push @terms, \&variable;
    push @terms, \&literal;
*{'_open'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::constant( "open" )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'varscalar', \&{'Grammar::Perl6::varscalar'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _open =>  match::get( $_[0], '$()' ) , } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_open;
    push @terms, \&_open;
*{'_print_with_fh'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "print" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "say" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "warn" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "die" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'indirect_object', \&{'Grammar::Perl6::indirect_object'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _print_with_fh =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_print_with_fh;
*{'_print'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "print" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "say" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "warn" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "die" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'list', \&{'Grammar::Perl6::list'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _print =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_print;
*{'_my'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "my" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "our" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "local" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _my =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_my;
*{'_simple_statement'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'op', 
           Runtime::Perl5::RuleOps::concat(
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "die" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\." )
                 ,
             ] )
           ,
             Runtime::Perl5::RuleOps::constant( "\." )
           ,
             Runtime::Perl5::RuleOps::constant( "\." )
           ,
           )
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _simple_statement =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_simple_statement;
*{'sub_decl'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "sub" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::capture( 'fix', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "infix" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "prefix" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "postfix" )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
         Runtime::Perl5::RuleOps::constant( "\<" )
       ,
         Runtime::Perl5::RuleOps::capture( 'id', 
             Runtime::Perl5::RuleOps::non_greedy_star(
                 \&{'Grammar::Perl6::any'}
               ,
             )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\>" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'block', \&{'Grammar::Perl6::block'} )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_decl =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&sub_decl;
*{'sub_defin'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "sub" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'block', \&{'Grammar::Perl6::block'} )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_defin =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&sub_defin;
*{'term2'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'term1', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( \@ops )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'term2', 
             Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
           ,
         )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_application_term =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
*{'sub_application'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::capture( 'term1', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
                 ,
                   Runtime::Perl5::RuleOps::capture( 'term2', \&{'Grammar::Perl6::term2'} )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'op', 
             Runtime::Perl5::RuleOps::alternation( \@ops )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'term2', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
                 ,
                   Runtime::Perl5::RuleOps::capture( 'term2', \&{'Grammar::Perl6::term2'} )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { sub_application =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&sub_application;
*{'eval_perl5'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "eval" )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::capture( 'literal', \&{'Grammar::Perl6::literal'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\," )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
         Runtime::Perl5::RuleOps::constant( "lang" )
       ,
         Runtime::Perl5::RuleOps::constant( "\<" )
       ,
         Runtime::Perl5::RuleOps::constant( "perl5" )
       ,
         Runtime::Perl5::RuleOps::constant( "\>" )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
         Runtime::Perl5::RuleOps::optional(
             Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { eval_perl5 =>  match::get( $_[0], '$<literal>' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&eval_perl5;
*{'_return'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "return" )
       ,
         \&{'Grammar::Perl6::p6ws'}
       ,
         Runtime::Perl5::RuleOps::capture( 'val', 
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::capture( 'term1', \&{'Grammar::Perl6::term1'} )
                 ,
                   Runtime::Perl5::RuleOps::capture( 'term2', \&{'Grammar::Perl6::term2'} )
                 ,
             ] )
           ,
         )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\;" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { _return =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @statements, \&_return;
