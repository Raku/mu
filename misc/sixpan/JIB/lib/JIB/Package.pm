package JIB::Package;

use strict;
use warnings;

my $package_re =qr/^(\w+)      - # prefix
					([\w-]+?)  - # package name
					([\d.]+)   - # version
					(\w+\+\S+) $ # authority
				/smx;

sub new {
	my $class = shift;
	my $self = {
		changed_distname => 0
	};
	bless $self, $class;
	return $self->init(@_) ? $self : undef;
}

sub init {
	my ($self, $args) = @_;
	if ($args && ref $args eq 'HASH') {
		for my $arg (keys %{$args}) {
			next unless $arg;
			die __PACKAGE__ . " doesn't have an accessor method $arg" unless $self->can($arg);
			$self->$arg($args->{$arg});
		}
	}

	$self->parse_name() if $self->{changed_distname};

	return 1;
}

sub distname {
	my $self = shift;

	if (@_) {
		$self->{distname} = shift;
		$self->{changed_distname} = 1;
	}

	return $self->{distname};
}

sub prefix {
	my $self = shift;

	$self->parse_name() if $self->{changed_distname};
	return $self->{prefix};
}

sub name {
	my $self = shift;

	$self->parse_name() if $self->{changed_distname};
	return $self->{name};
}

sub version {
	my $self = shift;

	$self->parse_name() if $self->{changed_distname};
	return $self->{version};
}

sub authority {
	my $self = shift;
	$self->parse_name() if $self->{changed_distname};
	return $self->{authority};
}

sub parse_name {
	my $self = shift;

	($self->{prefix},
	 $self->{name},
	 $self->{version},
	 $self->{authority}) = $self->{distname} =~ $package_re;

	$self->{changed_distname} = 0;
}

1;
