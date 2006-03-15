# pX/Common/p6rule.pl - fglock
#
# experimental implementation of p6-regex parser
#

package Pugs::Grammar::Rule;

use Text::Balanced; 
use Data::Dumper;
use Pugs::Runtime::Rule;
#use Pugs::Runtime::Grammar; -- MOP 

use base Pugs::Grammar::Base;

use strict;
use warnings;
no warnings qw( once redefine );

use vars qw( @rule_terms );

sub subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)$/s;
    return unless defined $code;
    #print "parsing subrule $code\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { subrule => $code } ],
    }
}

# XXX - set non-capture flag
sub non_capturing_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\?(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "non_capturing_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { non_capturing_subrule => $code } ],
    }
}

sub negated_subrule {
    my ( $code, $tail ) = $_[0] =~ /^\<\!(.*?)\>(.*)$/s;
    return unless defined $code;
    # print "negated_subrule $code - $1\n";
    return { 
        bool  => 1,
        match => { code => $code },
        tail  => $tail,
        capture => [ { negated_subrule => $code } ],
    }
}

*capturing_group = 
    do{ package Pugs::Runtime::Rule;
      concat(
        constant( '(' ),
        capture( 'capturing_group',
            \&Pugs::Grammar::rule,
        ),
        constant( ')' )
      )
    };

@rule_terms = (
    \&capturing_group,

    # <'literal'> literal \*
    ruleop::concat(    
        ruleop::constant( "<\'" ),
        ruleop::capture( 'constant',
            ruleop::non_greedy_star( \&any ),
        ),
        ruleop::constant( "\'>" ),
    ),

    \&negated_subrule,
    \&non_capturing_subrule,
    \&subrule,
);

# <ws>* [ <closure> | <subrule> | ... ]
*term = 
    Pugs::Runtime::Rule::concat(
        \&ws_star,
        Pugs::Runtime::Rule::alternation( \@rule_terms ),
        \&ws_star,
    );

# XXX - allow whitespace everywhere

# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    ruleop::alternation( [
        ruleop::capture( 'star', 
            ruleop::concat(
                ruleop::capture( 'term', \&term ),
                ruleop::capture( 'literal',
                    ruleop::alternation( [
                        ruleop::constant( '??' ),
                        ruleop::constant( '?' ),
                        ruleop::constant( '*?' ),
                        ruleop::constant( '+?' ),
                        ruleop::constant( '*' ),
                        ruleop::constant( '+' ),
                    ] ),
                ),
                \&ws_star,
            ),
        ),
        \&term,
    ] );

*alt = 
    ruleop::capture( 'alt', 
        ruleop::concat(
            ruleop::capture( 'term', \&quantifier ),
            ruleop::greedy_plus(
                ruleop::concat(
                    ruleop::constant( '|' ),
                    ruleop::capture( 'term', \&quantifier ),
                ),
            ),
        ),
    ),                
;

sub p6ws {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'p6ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/s;
    return;
};
sub newline {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'newline'=> $1 },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\n)/s;
    return;
};
sub escaped_char {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'escaped_char'=> $1 },
        tail  => substr($_[0],2),
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^\\(.)/s;
    return;
};


sub code {
    return unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ $extracted ] ) : () ),
    };
}

sub literal {
    return unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ { literal => $extracted } ] ) : () ),
    };
}

sub ws_star {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s*)(.*)$/s;
    return;
};
sub p6ws_star {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'p6ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^((?:\s|\#(?-s:.)*)*)(.*)$/s;
    return;
};

sub variable {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws*'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => $1 ) : () ),
    }
        if $_[0] =~ / ^  
            (   [ $ % @ % ]
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

sub ident {
    #return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ident'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ { ident => $1 } ] ) : () ),
    }
        if $_[0] =~ / ^  
            ( 
                (?: 
                    (?:\:\:)? 
                    [_[:alnum:]]+ 
                )+
            )  
            (.*) $ /xs;
    return;
};

# delay execution
{
    local $SIG{__WARN__} = sub {};
    require Pugs::Grammar::Rule::Rule;
}

1;
