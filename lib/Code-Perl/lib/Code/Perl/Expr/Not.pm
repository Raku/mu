# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Not.pm,v 1.2 2003/06/17 17:43:39 fergal Exp $

use strict;

package Code::Perl::Expr::Not;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
	get_set => [qw( -java Expr )]
);

sub eval
{
	my $self = shift;

	my $expr = $self->getExpr;

	return ! $expr->eval;
}

sub perl
{
	my $self = shift;

	my $expr = $self->getExpr->perl;

	return "! ($expr)";
}

1;
