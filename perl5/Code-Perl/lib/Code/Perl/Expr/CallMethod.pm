# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/CallMethod.pm,v 1.4 2003/06/17 18:11:41 fergal Exp $

use strict;

package Code::Perl::Expr::CallMethod;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
	get_set => [qw( -java Object MethodName Args )]
);

sub eval
{
	my $self = shift;

	my $object = $self->getObject;
	my $methodname = $self->getMethodName->eval;
	my $args = $self->getArgs;

	return $object->eval->$methodname($args->eval);
}

sub perl
{
	my $self = shift;

	my $object = $self->getObject->perl;
	my $methodname = $self->getMethodName->perl;
	my $args = $self->getArgs->perl;

	return "($object)->$methodname($args)";
}

1;
