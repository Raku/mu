# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Infix.pm,v 1.2 2003/06/17 17:43:39 fergal Exp $

use strict;

package Code::Perl::Expr::Infix;

use base qw( Code::Perl::Expr::Base );

use Class::MethodMaker (
    get_set => [qw( -java Exprs Op )]
);

sub perl
{
    my $self = shift;

    my $op = $self->getOp;
    my @exprs = map {"(".$_->perl.")"} @{$self->getExprs};

    return join(" $op ", @exprs);
}

1;
