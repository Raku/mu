# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Base.pm,v 1.1 2003/06/17 15:14:55 fergal Exp $

use strict;

package Code::Perl::Base;

sub new
{
	my $pkg = shift;

	my %args = @_;
	my $self = bless \%args, $pkg;

	$self->init;

	return $self;
}

sub init
{
}

1;
