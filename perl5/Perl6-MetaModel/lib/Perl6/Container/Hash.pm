package Perl6::Container::Hash;

use strict;
use warnings;

use Perl6::Container::Scalar;

sub mk_containers { # consolidate with Array
	map {
		my $c = Perl6::Container::Scalar->new();
		$c->STORE($_);
		$c;
	} @_;
}

sub new {
	my $pkg = shift;

	my $self = bless {
		data => {},
	}, $pkg;

	$self;
}

sub hash_fetch {
	my $self = shift;
	$self->{data};
}
sub hash_store {
	my $self = shift;
	my $hash = shift;
	$self->{data} = $hash;	
}
sub hash_fetchElem {
	my $self = shift;
	my $idx = shift;
	$self->hash_fetch->{$idx};
}
sub hash_storeElem {
	my $self = shift;
	my $idx = shift;
	my $container = shift;
	$self->hash_fetch->{$idx} = $container;
}
sub _hash_vivifyElem {
	my $self = shift;
	my $idx = shift;
	my $c = $self->hash_fetchElem($idx);
	unless ($c){
		$c = Perl6::Container::Scalar->new;
		$self->hash_storeElem($idx, $c);
	}
	$c;
}
sub hash_fetchVal {
	my $self = shift;
	my $idx = shift;
	my $c = $self->hash_fetchElem($idx);
	return undef unless $c;
	$c->FETCH;
}
sub hash_storeVal {
	my $self = shift;
	my $idx = shift;
	my $value = shift;
	$self->_hash_vivifyElem($idx)->STORE($value);
}
sub hash_fetchKeys {
	my $self = shift;
	keys %{ $self->hash_fetch };
}
sub hash_deleteElem {
	my $self = shift;
	my $idx = shift;
	delete $self->hash_fetch->{$idx};
}
sub hash_existsElem {
	my $self = shift;
	my $idx = shift;
	exists $self->hash_fetch->{$idx};
}
sub hash_clear {
	my $self = shift;
	%{$self->hash_fetch} = ();
}
sub hash_isEmpty {
	my $self = shift;
	# FIXME context should be propagated to methods
	scalar(() = $self->hash_fetchKeys) == 0;
}

1;
