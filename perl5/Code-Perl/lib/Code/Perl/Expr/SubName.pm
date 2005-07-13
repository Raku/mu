# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/SubName.pm,v 1.1 2003/06/17 15:11:11 fergal Exp $

use strict;

package Code::Perl::Expr::SubName;

use base 'Code::Perl::Expr::Constant';

use Carp 'confess';

sub eval
{
	my $self = shift;

	return $self->getValue
}

sub perl
{
	my $self = shift;

	return $self->getValue;
}

1;
