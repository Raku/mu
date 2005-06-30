# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/DerefArray.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::DerefArray;

use base qw( Code::Perl::Expr::Base );

use Class::MethodMaker (
	get_set => [qw( -java Index Ref )]
);

sub eval
{
	my $self = shift;

	my $array = $self->getRef->eval;

	return $array->[$self->getIndex->eval];
}

sub perl
{
	my $self = shift;

	my $array_perl = $self->getRef->perl;

	my $index = $self->getIndex->perl;
	return "($array_perl)->[$index]";
}

1;
