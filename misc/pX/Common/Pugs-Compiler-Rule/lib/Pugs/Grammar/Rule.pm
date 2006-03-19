# Pugs::Grammar::Rule - fglock
#
# the 'Rule' grammar - 'rule' is the main rule
#

package Pugs::Grammar::Rule;

use Text::Balanced; 
use Data::Dumper;
use Pugs::Runtime::Rule;
#use Pugs::Runtime::Grammar; -- MOP 

use strict;
use warnings;
no warnings qw( once redefine );

use vars qw( @rule_terms );
use Pugs::Grammar::Rule::Rule;   # compiled with lrep

sub code {
    my $class = shift;
    return unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    #warn "Testing <code>: #$_[0]#" if $extracted ne '';
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
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    };
}

# delay execution
{
    local $SIG{__WARN__} = sub {};
    require Pugs::Grammar::Rule::Rule;
}

# curry @rule_terms with Grammar
@rule_terms = map { 
        my $method = $_;
        sub{ 
            #warn "Trying $method\n";
            my $match = Pugs::Grammar::Rule->$method(@_);
            #warn $match->{bool} ? "Match ".Dumper($match) : "No match\n";
            return $match;
        }
    }
    @rule_terms;
1;
