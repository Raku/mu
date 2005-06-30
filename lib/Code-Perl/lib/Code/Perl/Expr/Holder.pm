# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Holder.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::Holder;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
	get_set => [qw( -java Expr )]
);

sub eval
{
	my $self = shift;

	return $self->getExpr->eval;
}

sub perl
{
	my $self = shift;

	return $self->getExpr->perl;

}
1;
