# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/List.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::List;

use base 'Code::Perl::Expr::Constant';

sub eval
{
	my $self = shift;

	return map { $_->eval } @{$self->getValue};
}

sub perl
{
	my $self = shift;

	return join(", ", map {$_->perl} @{$self->getValue});
}

1;
