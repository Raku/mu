package Pugs::Grammar::Prefix;
use strict;
use warnings;
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Infix;

sub add_rule {
    my $self = shift;
    my %opt = @_;
    my $name2 = $opt{name};
    $name2 =~ s/\\/\\\\/g;
    #print "Add Prefix $name2 \n";
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'prefix', 
        assoc => 'non',
        name => $name2,
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        precedence => 'equal',
        other  => $name2,
        fixity => 'prefix', 
        assoc => 'non',
        name => 'prefix:<' . $name2 . '>',
    );
    my $name = quotemeta( $opt{name} );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $name . '" ,} }' );
    $self->SUPER::add_rule( 
        "prefix:<" . $opt{name} . ">",
        '{ return { op => "prefix:<' . $name . '>" ,} }' );
}


sub add_same_precedence_ops {
    my ($class, $opt, $other, @ops) = @_;
    $class->add_rule(other => $other, precedence => 'equal', name => $_, %$opt) for @ops;
}

BEGIN {
    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        precedence => 'tighter',
        other => 'infix:<*>',
    );
    __PACKAGE__->add_same_precedence_ops( { assoc => 'left'}, 'prefix:<+>', qw(
        - ? ~ | @ % $ & ! * : = \\
    ) );
    __PACKAGE__->add_rule( 
        name => '++',
        assoc => 'left',
        precedence => 'tighter',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '--',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<++>',
    );
    # experimental
    __PACKAGE__->add_rule(
        name => 'try',
        assoc => 'non',
        precedence => 'equal',
        other => 'infix:<+>',
    );

    __PACKAGE__->recompile;
}

1;
