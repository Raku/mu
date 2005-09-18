#!/usr/bin/perl

package Blondie::ADTs;

use strict;
use warnings;

{
	package Blondie::Unit;

	sub new {
		my $class = shift;
		my $value = shift;

		bless \$value, $class;
	}

	sub val { ${ $_[0] } }

	sub str { "${ $_[0] }" }

	sub fmap {
		my $self = shift;
		my $f = shift;
		my $res = (ref $self)->new($f->($self->val));
	}
}

{
	package Blondie::List;

	sub new {
	my $class = shift;
		bless [@_], $class;
	}

	sub str { map { "$_" } @{ $_[0] } }

	sub fmap {
	my $self = shift;
	my $f = shift;
		(ref $self)->new(map { &$f($_) } @$self);
	}

	sub values { @{ $_[0] } }
}

{
	package Blondie::Map;

	sub new {
		my $class = shift;
		bless {@_}, $class;
	}

	sub AUTOLOAD {
		our $AUTOLOAD =~ /([^:]+)$/;
		my $method = $1;

		my $a = sub { $_[0]{$method} };

		{
			no strict 'refs';
			*{$method} = $a;
		}

		$_[0]->$a;
	}

	sub can {
		my $self = shift;
		my $key = shift;

		exists $self->{$key} || $self->SUPER::can($key);
	}

	sub get {
		$_[0]{$_[1]}
	}

	sub fmap {
		my $self = shift; my $f = shift;
		(ref $self)->new( map { $_ => &$f($self->get($_)) } $self->keys );
	}

	sub values { values %{ $_[0] } }

	sub keys { keys %{ $_[0] } }

	sub str {
		map { "$_ => $_[0]{$_}" } $_[0]->keys;
	}

	sub merge {
		my $self = shift;
		my $other = shift;

		(ref $self)->new(%$self, %$other);
	}
}

{
	package Blondie::Env;
	use base qw/Blondie::Map/;
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::ADTs - Reused abstract data types.

=head1 SYNOPSIS

	use Blondie::ADTs;

	# Blondie::Map, Blondie::Unit and Blondie::List are defined

=head1 DESCRIPTION

These ADTs are the implementation classes of all the L<Blondie::Nodes> and some
more objects.

They provide stringification, fmapping and value encapsulation features.

They are discussed in the main L<Blondie> documentation.

=cut


