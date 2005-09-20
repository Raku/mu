#!/usr/bin/perl

package Blondie::Backend::C::Builtins;

use strict;
use warnings;

use Blondie::Nodes;

{
	package Blondie::Backend::C::Prim;
	use base qw/Blondie::Backend::Prim/;
}

sub native {
    my $params = @_ == 1 ? shift : { @_ };

    my $node = Blondie::Prelude->env->get($params->{name});

    my $digest = $node->digest;

    $digest => Blondie::Backend::C::Prim->new(
		equals => $digest,
		body => $params->{body},
		arity => $params->{arity},
		name => $params->{name},
		type => $params->{type},
    );
}

sub type {[ map { Blondie::TypeSafe::Type::Std->new($_) } @_ ]}

my %by_digest = (
	map { native($_) } (
		{
			arity => 0,
			name => '$*OUT',
			body => 'stdout',
			type => Blondie::TypeSafe::Type::Std->new("GV"),
		},
		{
			arity => 3,
			name => '&print',
			body => 'prel_print',
			type => type(qw/GV PV/ => "IV"),
		},
		{
			arity => 2,
			name => '&infix:<~>',
			body => 'prel_cat',
			type => type(qw/PV PV/ => "PV"),
		},
	)
);

sub find {
    my $class = shift;
    my $digest = shift;

    $by_digest{$digest};
}

sub prelude {
<<C
#include <stdio.h>
#include <gc.h>

typedef int IV;
typedef FILE * GV;
typedef char * PV;

int prel_print (GV f, const PV c) {
	return fprintf(f, "%s", c);
}

PV prel_cat (PV x, PV y) {
	char * target = (char *)GC_MALLOC(sizeof(char) * (strlen(x) + strlen(y) + 1));
	(void)sprintf(target, "%s%s", x, y);
	return target;
}

PV type_convert_IV_PV (IV x) {
	char * target = (char *)GC_MALLOC(sizeof(char) * 100);
	(void)sprintf(target, "%d", x);
	return target;
}
C
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::C::Builtins - 

=head1 SYNOPSIS

	use Blondie::Backend::C::Builtins;

=head1 DESCRIPTION

=cut


