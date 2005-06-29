use strict;
use warnings;

use Perl6::MetaModel;

use P5::PIL::Run::Container::Scalar;

sub mk_containers { # consolidate with Array
	map {
		my $c = P5::PIL::Run::Container::Scalar->new_instance();
		$c->scalar_store($_);
		$c;
	} @_;
}

role 'IHash' => {};

class 'P5::PIL::Run::Container::Hash' => {
	does => [ 'IHash' ],
	class => {
		attrs => [ '%:slots' ],
		init => sub {
			my $self = shift;
			$self->set_value('%:slots', {});
		},
		methods => {
			hash_fetch => sub {
				my $self = shift;
				$self->get_value('%:slots');
			},
			hash_store => sub {
				my $self = shift;
				my $hash = shift;
				$self->set_value('%:slots', $hash);
			},
			hash_fetchElem => sub {
				my $self = shift;
				my $idx = shift;
				$self->hash_fetch->{$idx};
			},
			hash_storeElem => sub {
				my $self = shift;
				my $idx = shift;
				my $container = shift;
				$self->hash_fetch->{$idx} = $container;
			},
			hash_fetchVal => sub {
				my $self = shift;
				my $idx = shift;
				$self->hash_fetchElem($idx)->scalar_fetch;
			},
			hash_storeval => sub {
				my $self = shift;
				my $idx = shift;
				my $value = shift;
				$self->hash_fetchElem($idx)->scalar_store($value);
			},
			hash_fetchKeys => sub {
				my $self = shift;
				keys %{ $self->hash_fetch };
			},
			hash_deleteElem => sub {
				my $self = shift;
				my $idx = shift;
				delete $self->hash_fetch->{$idx};
			},
			hash_existsElem => sub {
				my $self = shift;
				my $idx = shift;
				exists $self->hash_fetch->{$idx};
			},
			hash_clear => sub {
				my $self = shift;
				%{$self->hash_fetch} = ();
			},
			hash_isEmpty => sub {
				my $self = shift;
				scalar $self->fetchKeys;
			},
		},
	},
};

1;
