package Pugs::Grammar::Infix;
use strict;
use warnings;
#use base qw(Pugs::Grammar::Operator);
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

sub add_rule {
    # print "add infix operator\n";
    my $self = shift;
    my %opt = @_;
    #print "Infix add: @{[ %opt ]} \n";

    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
    );
    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
        name => 'infix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        "infix:<" . $opt{name} . ">",
        '{ return { op => "infix:<' . $opt{name} . '>" ,} }' );
}

BEGIN {
    __PACKAGE__->add_rule( 
        name => '*',
        assoc => 'left',
    );
    __PACKAGE__->add_rule( 
        name => '/',
        assoc => 'left',
        precedence => 'equal',
        other => '*',
    );

    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        precedence => 'looser',
        other => '*',
    );
    __PACKAGE__->add_rule( 
        name => '-',
        assoc => 'left',
        precedence => 'equal',
        other => '+',
    );
    __PACKAGE__->add_rule( 
        name => '~',
        assoc => 'left',
        precedence => 'equal',
        other => '+',
    );
    
    __PACKAGE__->add_rule( 
        name => '..',
        assoc => 'left',
        precedence => 'looser',
        other => '+',
    );
    
    __PACKAGE__->add_rule( 
        name => '|',
        assoc => 'left',
        precedence => 'looser',
        other => '..',
    );

    # XXX - BUG - collision with &name
    
    #__PACKAGE__->add_rule( 
    #    name => '&',
    #    assoc => 'left',
    #    precedence => 'equal',
    #    other => 'infix:<|>',
    #);

    __PACKAGE__->add_rule( 
        name => 'eq',
        assoc => 'left',
        precedence => 'looser',
        other => 'infix:<|>',
    );
    __PACKAGE__->add_rule( 
        name => 'ne',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '==',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '!=',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '+=',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '-=',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '<',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '>',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    
    __PACKAGE__->add_rule( 
        name => '<=',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    __PACKAGE__->add_rule( 
        name => '>=',
        assoc => 'left',
        precedence => 'equal',
        other => 'eq',
    );
    
    __PACKAGE__->add_rule( 
        name => '&&',
        assoc => 'right',
        precedence => 'looser',
        other => 'eq',
    );
    
    __PACKAGE__->add_rule( 
        name => '||',
        assoc => 'right',
        precedence => 'looser',
        other => '&&',
    );
    __PACKAGE__->add_rule( 
        name => '//',
        assoc => 'right',
        precedence => 'equal',
        other => '||',
    );
    
    __PACKAGE__->add_rule( 
        name => '=',
        assoc => 'right',
        precedence => 'looser',
        other => '||',
    );
    __PACKAGE__->add_rule( 
        name => ':=',
        assoc => 'right',
        precedence => 'equal',
        other => '=',
    );
    __PACKAGE__->add_rule( 
        name => '~=',
        assoc => 'right',
        precedence => 'equal',
        other => '=',
    );
    __PACKAGE__->add_rule( 
        name => '=~',
        assoc => 'right',
        precedence => 'equal',
        other => '=',
    );
    
    __PACKAGE__->add_rule( 
        name => 'Y',
        assoc => 'list',
        precedence => 'looser',
        other => '=',
    );
    __PACKAGE__->add_rule( 
        name => '¥',
        assoc => 'list',
        precedence => 'equal',
        other => 'Y',
    );
    __PACKAGE__->add_rule( 
        name => ',',
        assoc => 'list',
        precedence => 'equal',
        other => 'Y',
    );
    
    __PACKAGE__->add_rule( 
        name => 'and',
        assoc => 'left',
        precedence => 'looser',
        other => 'Y',
    );
    
    __PACKAGE__->add_rule( 
        name => 'or',
        assoc => 'left',
        precedence => 'looser',
        other => 'and',
    );
    __PACKAGE__->add_rule( 
        name => 'err',
        assoc => 'left',
        precedence => 'looser',
        other => 'and',
    );
    
    #__PACKAGE__->add_rule( 
    #    name => ';',
    #    assoc => 'list',
    #    precedence => 'looser',
    #    other => 'or',
    #);
    
    # '->' is not an operator
    #__PACKAGE__->add_rule( 
    #    name => '->',
    #    assoc => 'non',
    #    precedence => 'equal',
    #    other => ';',
    #);

    # experimental
    #__PACKAGE__->add_rule( 
    #    name => 'IF',
    #    assoc => 'non',
    #    precedence => 'tighter',
    #    other => ';',
    #);

    __PACKAGE__->recompile;
}

1;
