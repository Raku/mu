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
block:
        '->' exp 'BLOCK_START' exp '}'        
        { $_[0]->{out}= { 'pointy_block' => $_[4], signature => $_[2], } }
    |   'BLOCK_START' exp '}'        
        { $_[0]->{out}= { 'bare_block' => $_[2] } }
    ;
    
attr:
        #empty  
        { $_[0]->{out}= { attribute => [] } }
    |   BAREWORD  BAREWORD  attr
        { $_[0]->{out}= { 
            attribute => [ 
                [$_[1], $_[2],],
                @{$_[3]{attribute}}, 
            ], 
        } }
    ;

type:
        BAREWORD
        { $_[0]->{out}= $_[1] }
    |   BAREWORD '|' type
        { $_[0]->{out}= { op1 => $_[2], exp1 => $_[1], exp2 => $_[3] } }
    ;
    
signature_term:
        NUM 
        { $_[0]->{out}= $_[1] }
    |   type signature_term
        { $_[0]->{out}= { type => $_[1], %{$_[2]} } }
    |   ':' signature_term
        { $_[0]->{out}= { named_only => 1, %{$_[2]} } }
    |   '*' signature_term
        { $_[0]->{out}= { splat => 1, %{$_[2]} } }
    |   signature_term '?'
        { $_[0]->{out}= { optional => 1, %{$_[1]} } }
        
    # XXX - infinite loop
    # |   signature_term attr
    #     { $_[0]->{out}= { %{$_[2]}, %{$_[1]} } }
    
    |   signature_term BAREWORD BAREWORD
        { $_[0]->{out}= { attribute => [
                [$_[2], $_[3],],
            ], %{$_[1]} } }
    ;

signature:
        #empty  
        { $_[0]->{out}= { signature => [] } }
    |   signature_term 
        { $_[0]->{out}= { signature => [ $_[1] ] } }
    |   signature ',' signature
        { $_[0]->{out}= { 
            signature => [ 
                @{$_[1]{signature}}, 
                @{$_[3]{signature}}, 
            ], 
        } }
    ;

stmt:  
      IF exp block 
        { $_[0]->{out}= { op1 => $_[1], exp1 => $_[2], exp2 => $_[3] } }
    | IF exp block 'else' block
        { $_[0]->{out}= { op1 => $_[1], exp1 => $_[2], exp2 => $_[3], exp3 => $_[5] } }
    | IF exp block 'elsif' exp block 'else' block
        { $_[0]->{out}= { 
            op1 => $_[1], exp1 => $_[2], exp2 => $_[3], 
                          exp3 => $_[5], exp4 => $_[6],
                          exp5 => $_[8],
        } }
    | IF exp block 'elsif' exp block 
        { $_[0]->{out}= { 
            op1 => $_[1], exp1 => $_[2], exp2 => $_[3], 
                          exp3 => $_[5], exp4 => $_[6],
        } }

    | 'for' exp block
        { $_[0]->{out}= { op1 => $_[1], exp1 => $_[2], exp2 => $_[3], } }

    | SUB BAREWORD             attr block 
        { $_[0]->{out}= { op1 => $_[1], name => $_[2], block => $_[4], %{$_[3]} } }
    | SUB BAREWORD '(' signature ')' attr block 
        {
            #print "parse-time define sub: ", Dumper( $_[2] );
            #push @subroutine_names, $_[2]->{bareword};
            #print "Subroutines: @subroutine_names\n";
            $_[0]->{out}= { op1 => $_[1], name => $_[2], block => $_[7], %{$_[4]}, %{$_[6]} } 
        }
    
    | SUB SUB BAREWORD             attr block 
        { $_[0]->{out}= { op1 => $_[2], name => $_[3], block => $_[5], %{$_[4]} } }
    | SUB SUB BAREWORD '(' signature ')' attr block 
        {
            #print "parse-time define sub: ", Dumper( $_[2] );
            #push @subroutine_names, $_[2]->{bareword};
            #print "Subroutines: @subroutine_names\n";
            $_[0]->{out}= { op1 => $_[2], name => $_[3], block => $_[8], %{$_[5]}, %{$_[7]} } 
        }

    | block        
        { $_[0]->{out}= $_[1] }
    | 'TRAIT' block     
        { $_[2]{trait} = $_[1]{trait}; $_[0]->{out}= $_[2] }
    ;
    
exp: 
      NUM                 
        { $_[0]->{out}= $_[1] }

    | '@' '(' exp ')' 
        { $_[0]->{out}= { op1 => 'array_context', exp1 => $_[3], } }

    | BAREWORD            
        { $_[0]->{out}= { op1 => 'call', sub => $_[1], } }

    | BAREWORD 'IF' exp   %prec P003 
        { $_[0]->{out}= { op1 => $_[2], exp1 => $_[3], 
            exp2 => { op1 => 'call', sub => $_[1], } } }

    | BAREWORD exp   %prec P003
        { $_[0]->{out}= { op1 => 'call', sub => $_[1], param => $_[2], } }
    | exp '.' BAREWORD    %prec P003
        { $_[0]->{out}= { op1 => 'method_call', self => $_[1], method => $_[3], } }
    | exp '.' BAREWORD '(' exp ')'  %prec P003
        { $_[0]->{out}= { op1 => 'method_call', self => $_[1], method => $_[3], param => $_[5], } }
    | exp '.' BAREWORD exp   %prec P003
        { $_[0]->{out}= { op1 => 'method_call', self => $_[1], method => $_[3], param => $_[4], } }
        

    | MY NUM attr 
        { $_[0]->{out}= { 
            op1 => { op => $_[1]{stmt} }, 
            fixity => 'prefix', 
            exp1 => $_[2],
            %{$_[3]}, } }

    | stmt                
        { $_[0]->{out}= $_[1] }
    | stmt exp            
        { $_[0]->{out}= { op1 => ';', assoc => 'list', list => [ $_[1], $_[2] ] } }
    | exp ';' stmt        
        { $_[0]->{out}= { op1 => ';', assoc => 'list', list => [ $_[1], $_[3] ] } }
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
    #~ __PACKAGE__->add_rule( 
        #~ # tokenizer defined in Term.pm
        #~ name => 'CALL',
        #~ name2 => ')',
        #~ assoc => 'non',
        #~ precedence => 'tighter',
        #~ other => '*',
    #~ );

    __PACKAGE__->recompile;
}

1;
