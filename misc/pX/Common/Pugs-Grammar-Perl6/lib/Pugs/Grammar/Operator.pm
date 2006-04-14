package Pugs::Grammar::Operator;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Precedence;

our $operator;

BEGIN {
    $operator = Pugs::Grammar::Precedence->new( 
        grammar => 'Pugs::Grammar::Operator',
    );
    # print "created operator table\n";
}

sub add_rule {
    # print "add operator\n";
    my $self = shift;
    my %opt = @_;
    # print "Operator add: @{[ %opt ]} \n";

    delete $opt{rule};
    $operator->add_op( \%opt );
}

use Pugs::Grammar::Infix;
use Pugs::Grammar::Prefix;

# use Pugs::Runtime::Match;

# TODO - implement the "magic hash" dispatcher

our %hash;

sub recompile {
    my $class = shift;

    # tokenizer
    %hash = (
        %Pugs::Grammar::Infix::hash,
        %Pugs::Grammar::Prefix::hash,
    );
    $class->SUPER::recompile;

    # operator-precedence
    my $g = $operator->emit_yapp;
    #print $g;
    my $p = $operator->emit_grammar_perl5;

    # create a local variable '$out' inside the parser
    # $p =~ s/my\(\$self\)=/my \$out; my\(\$self\)=/;

    #print $p;
    eval $p;
    die "$@\n" if $@;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
