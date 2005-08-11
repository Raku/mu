# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Scalar.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::Scalar;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
	get_set => [qw( -java Name )]
);

sub eval
{
	my $self = shift;

	my $name = $self->getName;

	no strict 'refs';
	
	return ${$name};
}

sub perl
{
	my $self = shift;

	return '$'.$self->getName;
}
 
1;
