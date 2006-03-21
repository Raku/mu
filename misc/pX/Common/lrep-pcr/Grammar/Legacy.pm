package Grammar::Perl6;
*{'perl5_regex'} = 

    sub { 
        my $rule = 
         Runtime::Perl5::RuleOps::greedy_star(
             Runtime::Perl5::RuleOps::alternation( [
                   Runtime::Perl5::RuleOps::constant( "\." )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\|" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\*" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\+" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\(" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\)" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\[" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\]" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\?" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\:" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "s" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "w" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "_" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\\" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\^" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\$" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "n" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\#" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\-" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\<" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\>" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "\!" )
                 ,
                   Runtime::Perl5::RuleOps::constant( "alnum" )
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
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
         Runtime::Perl5::RuleOps::constant( "P5" )
       ,
         Runtime::Perl5::RuleOps::capture( 'p6ws', \&{'Grammar::Perl6::p6ws'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\{" )
       ,
         Runtime::Perl5::RuleOps::capture( 'perl5_regex', \&{'Grammar::Perl6::perl5_regex'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\}" )
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
    push @statements, \&perl5_rule_decl;
*{'word'} = sub {
    my $bool = $_[0] =~ /^([_[:alnum:]]+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'word'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'any'} = sub {
    my $bool = $_[0] =~ /^(.)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'any'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'escaped_char'} = sub {
    my $bool = $_[0] =~ /^\\(.)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'escaped_char'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'newline'} = sub {
    my $bool = $_[0] =~ /^(\n)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'newline'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'ws'} = sub {
    my $bool = $_[0] =~ /^(\s+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'p6ws'} = sub {
    my $bool = $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'p6ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
*{'non_capturing_subrule'} = sub {
    my $bool = $_[0] =~ /^\<\?(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'non_capturing_subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&non_capturing_subrule;
*{'negated_subrule'} = sub {
    my $bool = $_[0] =~ /^\<\!(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'negated_subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&negated_subrule;
*{'subrule'} = sub {
    my $bool = $_[0] =~ /^\<(.*?)\>(.*)$/sx;
    return {
        bool  => $bool,
        match => { 'subrule'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
};
    push @rule_terms, \&subrule;
*{'capturing_group'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::capture( 'rule', \&{'Grammar::Perl6::rule'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { capturing_group =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&capturing_group;
*{'constant'} = 

    sub { 
        my $rule = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\<" )
       ,
         Runtime::Perl5::RuleOps::capture( 'literal', \&{'Grammar::Perl6::literal'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\>" )
       ,
       )
    ;
        my $match = $rule->( @_ );
        return unless $match;
        my $capture_block = sub { return { constant =>  match::get( $_[0], '$()' )  } }       ,
; 
        #use Data::Dumper;
        #print "capture was: ", Dumper( $match->{capture} );
        return { 
            %$match,
            capture => [ $capture_block->( $match ) ],
        }; 
    }
;
    unshift @rule_terms, \&constant;
*{'term'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::alternation( \@Grammar::Perl6::rule_terms )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
       )
;
*{'const_word'} = 

    sub { 
        my $rule = 
         Runtime::Perl5::RuleOps::capture( 'word', \&{'Grammar::Perl6::word'} )
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
         Runtime::Perl5::RuleOps::capture( 'escaped_char', \&{'Grammar::Perl6::escaped_char'} )
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
         Runtime::Perl5::RuleOps::capture( 'capturing_group',
                 Runtime::Perl5::RuleOps::constant( "\." )
               ,
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
         Runtime::Perl5::RuleOps::greedy_star(
             Runtime::Perl5::RuleOps::alternation( [
                   \&{'Grammar::Perl6::alt'}
                 ,
                   \&{'Grammar::Perl6::quantifier'}
                 ,
             ] )
           ,
         )
       ,
;
*{'non_capturing_group'} = 
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\[" )
       ,
         \&{'Grammar::Perl6::rule'}
       ,
         Runtime::Perl5::RuleOps::constant( "\]" )
       ,
       )
;
    push @rule_terms, \&non_capturing_group;
*{'closure_rule'} = 

    sub { 
        my $rule = 
         Runtime::Perl5::RuleOps::capture( 'code', \&{'Grammar::Perl6::code'} )
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
         Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
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
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\<" )
       ,
         Runtime::Perl5::RuleOps::capture( 'variable', \&{'Grammar::Perl6::variable'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\>" )
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
       Runtime::Perl5::RuleOps::concat(
         Runtime::Perl5::RuleOps::constant( "\$" )
       ,
         Runtime::Perl5::RuleOps::capture( 'ident', \&{'Grammar::Perl6::ident'} )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\:" )
       ,
         Runtime::Perl5::RuleOps::constant( "\=" )
       ,
         Runtime::Perl5::RuleOps::optional(
             \&{'Grammar::Perl6::p6ws'}
           ,
         )
       ,
         Runtime::Perl5::RuleOps::constant( "\(" )
       ,
         Runtime::Perl5::RuleOps::capture( 'rule', \&{'Grammar::Perl6::rule'} )
       ,
         Runtime::Perl5::RuleOps::constant( "\)" )
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
