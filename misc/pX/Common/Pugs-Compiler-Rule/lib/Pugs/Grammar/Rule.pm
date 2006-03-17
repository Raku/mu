# Pugs::Grammar::Rule - fglock
#
# the 'Rule' grammar - 'rule' is the main rule
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
use Pugs::Grammar::Rule::Rule;   # compiled with lrep


# [ <term>[\*|\+] | <term> 
# note: <term>\* creates a term named 'star'
*quantifier = 
    Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::capture( 'star', 
            Pugs::Runtime::Rule::concat(
                Pugs::Runtime::Rule::capture( 'term', \&term ),
                Pugs::Runtime::Rule::capture( 'literal',
                    Pugs::Runtime::Rule::alternation( [
                        Pugs::Runtime::Rule::constant( '??' ),
                        Pugs::Runtime::Rule::constant( '?' ),
                        Pugs::Runtime::Rule::constant( '*?' ),
                        Pugs::Runtime::Rule::constant( '+?' ),
                        Pugs::Runtime::Rule::constant( '*' ),
                        Pugs::Runtime::Rule::constant( '+' ),
                    ] ),
                ),
                \&p6ws_star,
            ),
        ),
        \&term,
    ] );

*alt = 
    Pugs::Runtime::Rule::capture( 'alt', 
        Pugs::Runtime::Rule::concat(
            Pugs::Runtime::Rule::capture( 'term', \&quantifier ),
            Pugs::Runtime::Rule::greedy_plus(
                Pugs::Runtime::Rule::concat(
                    Pugs::Runtime::Rule::constant( '|' ),
                    Pugs::Runtime::Rule::capture( 'term', \&quantifier ),
                ),
            ),
        ),
    ),                
;

sub code {
    my $class = shift;
    return unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    };
}

sub literal {
    my $class = shift;
    return unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    };
}

sub variable {
    my $class = shift;
    #return unless $_[0];
    return { 
        bool  => 1,
        match => $1,
        tail  => $2,
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
    my $class = shift;
    #return unless $_[0];
    return { 
        bool  => 1,
        match => $1,
        tail  => $2,
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

1;
