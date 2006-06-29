package Pugs::Grammar::Perl6;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::Expression;
use Pugs::Grammar::Pod;

# TODO - redefine <ws> to test Pod.pm after each \n

sub parse {
    my $class = shift;
    my $src = shift;
    my ( $ast, $tail ) = Pugs::Grammar::Expression::ast( $src );
    return Pugs::Runtime::Match->new( { 
        bool  =>   1,
        match =>   $src,
        tail  =>   $tail,
        capture => $ast,
    } )
};

1;
