# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Number.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::Number;

use base 'Code::Perl::Expr::Constant';

# inherits eval

sub perl
{
	my $self = shift;

	return $self->getValue
}

1;
