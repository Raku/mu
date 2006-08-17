package Pugs::Grammar::Operator;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Precedence;

use Data::Dumper;

our $operator;

# TODO - implement the "magic hash" dispatcher
our %hash;

#our @subroutine_names;

BEGIN {
    $operator = Pugs::Grammar::Precedence->new( 
        grammar => 'Pugs::Grammar::Operator',
        header  => q!
  
exp: 
      NUM                 
        { $_[0]->{out} = $_[1] }

    | BAREWORD
        { $_[0]->{out} = {  op1    => 'call', 
                            sub    => $_[1], } }
    | BAREWORD exp       %prec P001
        { $_[0]->{out} = {  op1    => 'call', 
                            sub    => $_[1], 
                            param  => $_[2], } }
 
    | REDUCE exp         %prec P001
        { $_[0]->{out} = {  %{$_[1]}, 
                            param  => $_[2], } }
       
    | DOT_BAREWORD exp   %prec P001
        { $_[0]->{out} = {  op1    => 'method_call', 
                            self   => { 'scalar' => '$_' }, 
                            method => $_[1], 
                            param  => $_[2], } }
    | DOT_BAREWORD   
        { $_[0]->{out} = {  op1    => 'method_call', 
                            self   => { 'scalar' => '$_' }, 
                            method => $_[1], } }

!,
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
    
    #push @subroutine_names, $opt{name};
}

use Pugs::Grammar::Infix;
use Pugs::Grammar::Prefix;
use Pugs::Grammar::Postfix;
use Pugs::Grammar::Circumfix;
use Pugs::Grammar::Postcircumfix;
use Pugs::Grammar::Ternary;

sub recompile {
    my $class = shift;

    # tokenizer
    %hash = (
        %Pugs::Grammar::Infix::hash,
        %Pugs::Grammar::Prefix::hash,
        %Pugs::Grammar::Postfix::hash,
        %Pugs::Grammar::Circumfix::hash,
        %Pugs::Grammar::Postcircumfix::hash,
        %Pugs::Grammar::Ternary::hash,
    );
    $class->SUPER::recompile;

    {
        #no warnings 'recursion'; # doesn't seem to work here
        local $SIG{'__WARN__'} =
            sub { 
                warn $_[0] if $_[0] !~ /recursion/ 
            };
        #warn 'compiling grammar';
        # operator-precedence
        my $g = $operator->emit_yapp;
        #print $g;
        my $p;
        $p = $operator->emit_grammar_perl5;
        #print $p;
        eval $p;
        #warn 'compiled grammar';
    }
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
