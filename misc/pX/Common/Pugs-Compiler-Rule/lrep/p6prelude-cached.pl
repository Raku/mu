# generated file - do not edit!
package grammar1;
*{'perl5_regex'} = 
 
    sub { 
        my $rule = 
         ruleop::greedy_star(
             ruleop::alternation( [
                   ruleop::constant( "\." )
                 ,
                   ruleop::constant( "\|" )
                 ,
                   ruleop::constant( "\*" )
                 ,
                   ruleop::constant( "\+" )
                 ,
                   ruleop::constant( "\(" )
                 ,
                   ruleop::constant( "\)" )
                 ,
                   ruleop::constant( "\[" )
                 ,
                   ruleop::constant( "\]" )
                 ,
                   ruleop::constant( "\?" )
                 ,
                   ruleop::constant( "\:" )
                 ,
                   ruleop::constant( "s" )
                 ,
                   ruleop::constant( "w" )
                 ,
                   ruleop::constant( "_" )
                 ,
                   ruleop::constant( "\\" )
                 ,
                   ruleop::constant( "\^" )
                 ,
                   ruleop::constant( "\$" )
                 ,
                   ruleop::constant( "\@" )
                 ,
                   ruleop::constant( "\%" )
                 ,
                   ruleop::constant( "n" )
                 ,
                   ruleop::constant( "\#" )
                 ,
                   ruleop::constant( "\-" )
                 ,
                   ruleop::constant( "\<" )
                 ,
                   ruleop::constant( "\>" )
                 ,
                   ruleop::constant( "\!" )
                 ,
                   ruleop::constant( "alnum" )
                 ,
                   ruleop::constant( "digit" )
                 ,
             ] )
           ,
         )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { perl5_regex =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
*{'perl5_rule_decl'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "rule" )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'ident', \&{'grammar1::ident'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\:" )
       ,
         ruleop::constant( "P5" )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::constant( "\{" )
       ,
         ruleop::capture( 'perl5_regex', \&{'grammar1::perl5_regex'} )
       ,
         ruleop::constant( "\}" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { perl5_rule_decl =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    push @grammar1::statements, \&perl5_rule_decl;
*{'grammar1::word'} = sub {
    my $bool = $_[0] =~ /^([_[:alnum:]]+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'word'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::any'} = sub {
    my $bool = $_[0] =~ /^(.)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'any'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::escaped_char'} = sub {
    my $bool = $_[0] =~ /^\\(.)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'escaped_char'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::newline'} = sub {
    my $bool = $_[0] =~ /^(\n)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'newline'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::ws'} = sub {
    my $bool = $_[0] =~ /^(\s+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::p6ws'} = sub {
    my $bool = $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'p6ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'grammar1::non_capturing_subrule'} = sub {
    my $bool = $_[0] =~ /^\<\?(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'non_capturing_subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&non_capturing_subrule;
*{'grammar1::negated_subrule'} = sub {
    my $bool = $_[0] =~ /^\<\!(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'negated_subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&negated_subrule;
*{'grammar1::subrule'} = sub {
    my $bool = $_[0] =~ /^\<(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&subrule;
*{'const_word'} = 

    sub { 
        my $rule = 
         ruleop::capture( 'word', \&{'grammar1::word'} )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { constant =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&const_word;
*{'const_escaped_char'} = 

    sub { 
        my $rule = 
         ruleop::capture( 'escaped_char', \&{'grammar1::escaped_char'} )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { constant =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&const_escaped_char;
*{'dot'} = 

    sub { 
        my $rule = 
         ruleop::capture( 'capturing_group',
             ruleop::constant( "\." )
           ,
         )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { dot =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&dot;
*{'rule'} = 
         ruleop::greedy_star(
             ruleop::alternation( [
                   \&{'grammar1::alt'}
                 ,
                   \&{'grammar1::quantifier'}
                 ,
             ] )
           ,
         )
       ,
;
*{'non_capturing_group'} = 
       ruleop::concat(
         ruleop::constant( "\[" )
       ,
         \&{'grammar1::rule'}
       ,
         ruleop::constant( "\]" )
       ,
       )
;
    push @rule_terms, \&non_capturing_group;
*{'closure_rule'} = 

    sub { 
        my $rule = 
         ruleop::capture( 'code', \&{'grammar1::code'} )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { closure =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&closure_rule;
*{'variable_rule'} = 

    sub { 
        my $rule = 
         ruleop::capture( 'variable', \&{'grammar1::variable'} )
       ,
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { variable =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&variable_rule;
*{'runtime_alternation'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "\<" )
       ,
         ruleop::capture( 'variable', \&{'grammar1::variable'} )
       ,
         ruleop::constant( "\>" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { runtime_alternation =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&runtime_alternation;
*{'named_capture'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "\$" )
       ,
         ruleop::constant( "\<" )
       ,
         ruleop::capture( 'ident', \&{'grammar1::ident'} )
       ,
         ruleop::constant( "\>" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\:" )
       ,
         ruleop::constant( "\=" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\(" )
       ,
         ruleop::capture( 'rule', \&{'grammar1::rule'} )
       ,
         ruleop::constant( "\)" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { named_capture =>  match::get( $_[0], '$()' )  ,} }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&named_capture;
*{'immediate_statement_rule'} = 
       ruleop::concat(
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::alternation( \@grammar1::statements )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
       )
;
*{'grammar'} = 
         ruleop::greedy_star(
             ruleop::capture( 'immediate_statement_exec', \&{'grammar1::immediate_statement_exec'} )
           ,
         )
       ,
;
*{'rule_decl'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "rule" )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'ident', \&{'grammar1::ident'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\{" )
       ,
         ruleop::capture( 'rule', \&{'grammar1::rule'} )
       ,
         ruleop::constant( "\}" )
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
    push @grammar1::statements, \&rule_decl;
*{'grammar_name'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "grammar" )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'ident', \&{'grammar1::ident'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
*{'_push'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::capture( 'op', 
             ruleop::alternation( [
                   ruleop::constant( "push" )
                 ,
                   ruleop::constant( "unshift" )
                 ,
             ] )
           ,
         )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'variable', \&{'grammar1::variable'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\," )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'code', 
             ruleop::non_greedy_star(
                 \&{'grammar1::any'}
               ,
             )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::constant( "\=" )
       ,
         ruleop::alternation( [
               ruleop::constant( "pod" )
             ,
               ruleop::constant( "head1" )
             ,
               ruleop::constant( "kwid" )
             ,
               ruleop::constant( "for" )
             ,
         ] )
       ,
         ruleop::non_greedy_star(
             \&{'grammar1::any'}
           ,
         )
       ,
         ruleop::constant( "\=" )
       ,
         ruleop::constant( "cut" )
       ,
       )
;
    push @statements, \&pod;
*{'use_v6'} = 
       ruleop::concat(
         ruleop::constant( "use" )
       ,
         \&{'grammar1::p6ws'}
       ,
         ruleop::constant( "v6" )
       ,
         ruleop::constant( "\-" )
       ,
         ruleop::constant( "pugs" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\;" )
       ,
       )
;
    push @statements, \&use_v6;
*{'term1'} = 
         ruleop::alternation( \@grammar1::terms )
       ,
;
*{'list'} = 
       ruleop::concat(
         ruleop::greedy_star(
           ruleop::concat(
             ruleop::capture( 'term1', \&{'grammar1::term1'} )
           ,
             ruleop::optional(
                 \&{'grammar1::p6ws'}
               ,
             )
           ,
             ruleop::constant( "\," )
           ,
             ruleop::optional(
                 \&{'grammar1::p6ws'}
               ,
             )
           ,
           )
         )
       ,
         ruleop::optional(
             ruleop::capture( 'term1', \&{'grammar1::term1'} )
           ,
         )
       ,
       )
;
*{'block'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::constant( "\{" )
       ,
         ruleop::capture( 'list', 
             ruleop::greedy_star(
               ruleop::concat(
                 ruleop::optional(
                     \&{'grammar1::p6ws'}
                   ,
                 )
               ,
                 ruleop::alternation( \@grammar1::statements )
               ,
               )
             )
           ,
         )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\}" )
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
       ruleop::concat(
         ruleop::constant( "macro" )
       ,
         \&{'grammar1::p6ws'}
       ,
         ruleop::capture( 'prefix', 
             ruleop::capture( 'word', \&{'grammar1::word'} )
           ,
         )
       ,
         ruleop::constant( "\:" )
       ,
         ruleop::constant( "\<" )
       ,
         ruleop::capture( 'id', 
             ruleop::non_greedy_star(
                 \&{'grammar1::any'}
               ,
             )
           ,
         )
       ,
         ruleop::constant( "\>" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\(" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::capture( 'list', \&{'grammar1::list'} )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\)" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "is" )
       ,
         \&{'grammar1::p6ws'}
       ,
         ruleop::constant( "parsed" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\(" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\/" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::capture( 'rule', \&{'grammar1::rule'} )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\/" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\)" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::capture( 'code', \&{'grammar1::code'} )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { macro =>  match::get( $_[0], '$()' )  ,} }       ,
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
    push @terms, \&variable;
    push @terms, \&literal;
*{'_print'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::capture( 'op', 
             ruleop::alternation( [
                   ruleop::constant( "print" )
                 ,
                   ruleop::constant( "say" )
                 ,
                   ruleop::constant( "warn" )
                 ,
                   ruleop::constant( "die" )
                 ,
             ] )
           ,
         )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'list', \&{'grammar1::list'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::capture( 'op', 
             ruleop::alternation( [
                   ruleop::constant( "my" )
                 ,
                   ruleop::constant( "our" )
                 ,
                   ruleop::constant( "local" )
                 ,
             ] )
           ,
         )
       ,
         ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
       ,
         ruleop::capture( 'variable', \&{'grammar1::variable'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::capture( 'op', 
           ruleop::concat(
             ruleop::alternation( [
                   ruleop::constant( "die" )
                 ,
                   ruleop::constant( "\." )
                 ,
             ] )
           ,
             ruleop::constant( "\." )
           ,
             ruleop::constant( "\." )
           ,
           )
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::constant( "sub" )
       ,
         \&{'grammar1::p6ws'}
       ,
         ruleop::capture( 'fix', 
             ruleop::alternation( [
                   ruleop::constant( "infix" )
                 ,
                   ruleop::constant( "prefix" )
                 ,
                   ruleop::constant( "postfix" )
                 ,
             ] )
           ,
         )
       ,
         ruleop::constant( "\:" )
       ,
         ruleop::constant( "\<" )
       ,
         ruleop::capture( 'id', 
             ruleop::non_greedy_star(
                 \&{'grammar1::any'}
               ,
             )
           ,
         )
       ,
         ruleop::constant( "\>" )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::capture( 'block', \&{'grammar1::block'} )
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
*{'term2'} = 

    sub { 
        my $rule = 
       ruleop::concat(
         ruleop::capture( 'term1', 
             ruleop::capture( 'term1', \&{'grammar1::term1'} )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'op', 
             ruleop::alternation( \@grammar1::ops )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'term2', 
             ruleop::capture( 'term1', \&{'grammar1::term1'} )
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
       ruleop::concat(
         ruleop::capture( 'term1', 
             ruleop::alternation( [
                   ruleop::capture( 'term1', \&{'grammar1::term1'} )
                 ,
                   ruleop::capture( 'term2', \&{'grammar1::term2'} )
                 ,
             ] )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'op', 
             ruleop::alternation( \@grammar1::ops )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'term2', 
             ruleop::alternation( [
                   ruleop::capture( 'term1', \&{'grammar1::term1'} )
                 ,
                   ruleop::capture( 'term2', \&{'grammar1::term2'} )
                 ,
             ] )
           ,
         )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::constant( "eval" )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\(" )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::capture( 'literal', \&{'grammar1::literal'} )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\," )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\:" )
       ,
         ruleop::constant( "lang" )
       ,
         ruleop::constant( "\<" )
       ,
         ruleop::constant( "perl5" )
       ,
         ruleop::constant( "\>" )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\)" )
       ,
         ruleop::optional(
             ruleop::capture( 'p6ws', \&{'grammar1::p6ws'} )
           ,
         )
       ,
         ruleop::constant( "\;" )
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
       ruleop::concat(
         ruleop::constant( "return" )
       ,
         \&{'grammar1::p6ws'}
       ,
         ruleop::capture( 'val', 
             ruleop::alternation( [
                   ruleop::capture( 'term1', \&{'grammar1::term1'} )
                 ,
                   ruleop::capture( 'term2', \&{'grammar1::term2'} )
                 ,
             ] )
           ,
         )
       ,
         ruleop::optional(
             \&{'grammar1::p6ws'}
           ,
         )
       ,
         ruleop::constant( "\;" )
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
