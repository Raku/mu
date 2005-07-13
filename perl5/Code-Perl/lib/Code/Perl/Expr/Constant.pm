# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Constant.pm,v 1.2 2003/06/17 18:37:21 fergal Exp $

use strict;

package Code::Perl::Expr::Constant;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
	get_set => [qw( -java Value )]
);

sub eval
{
	my $self = shift;

	return $self->getValue;
}

my %esc = (
	"\\" => "\\\\",
	"\n" => "\\n",
	"\r" => "\\r",
	'"' => '\\"',
	'$' => '\\$',
	'@' => '\\@',
);

sub getQuotedValue
{
	my $self = shift;

	my $value = $self->{Value};

	if (1)
	{
		$value =~ s/([\\\n\r"\$\@])/$esc{$1}/g;
		return '"'.$value.'"';
	}
	else
	{
		return '"'.quotemeta($self->{Value}).'"';
	}
}

1;
