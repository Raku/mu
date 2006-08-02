# Pugs::Grammar::Rule - fglock
#
# the 'Rule' grammar - 'rule' is the main rule
#

package Pugs::Grammar::Rule;
use strict;
use warnings;
no  warnings qw( once redefine uninitialized );

use Text::Balanced; 
use Data::Dumper;
use Pugs::Runtime::LrepRule;
use Pugs::Runtime::LrepMatch;
#use Pugs::Runtime::Grammar; -- MOP 

use vars qw( @rule_terms );
use base 'Pugs::Grammar::LrepBase';
use Pugs::Grammar::Rule::Rule;   # compiled with lrep

sub code {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return Pugs::Runtime::LrepMatch->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    } );
}

sub literal {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::LrepMatch->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    } );
}

sub metasyntax {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_bracketed( $_[0], "<..>" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::LrepMatch->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        capture => { metasyntax => $extracted },
    } );
}
BEGIN {
    push @rule_terms, 'metasyntax';
}

BEGIN {
    # XXX - currying should be made automatically by <@xxx> runtime
    # curry @rule_terms with Grammar
    @rule_terms = map { 
        my $method = $_;
        sub{ 
            # warn "Trying $method\n";
            my $match = Pugs::Grammar::Rule->$method(@_);
            #warn "Match $method ".Dumper($match) if $match->{bool};
            return $match;
        }
    }
    @rule_terms;
}

1;
