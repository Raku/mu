# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Append.pm,v 1.2 2003/06/17 17:43:39 fergal Exp $

use strict;

package Code::Perl::Expr::Append;

use base qw( Code::Perl::Expr::Infix );

sub init
{
	my $self = shift;

	$self->setOp(".");
}

sub eval
{
	my $self = shift;

	my $exprs = $self->getExprs;
	return join("", map {$_->eval} @$exprs);
}

1;
