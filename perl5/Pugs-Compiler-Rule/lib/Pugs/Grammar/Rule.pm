# pX/Common/p6rule.pl - fglock
#
# experimental implementation of p6-regex parser
#

use strict;
use warnings;

package Pugs::Grammar::Rule;

use Text::Balanced; 
use Data::Dumper;
no warnings 'once';

use vars qw( @rule_terms );

# XXX - move these to prelude using rx:perl

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
    ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::capture( 'capturing_group',
            \&rule,
        ),
        ruleop::constant( ')' )
    );

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
    ruleop::concat(
        \&ws_star,
        ruleop::alternation( \@rule_terms ),
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

#--------


sub any { 
    return unless $_[0];
    return { 
        bool  => 1,
        match => { '.'=> substr($_[0],0,1) },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ substr($_[0],0,1) ] ) : () ),
    };
}
sub ws {
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s+)(.*)$/s;
    return;
};
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
eval ( 'use Pugs::Grammar::Rule::Rule' );
die $@ if $@;

1;
