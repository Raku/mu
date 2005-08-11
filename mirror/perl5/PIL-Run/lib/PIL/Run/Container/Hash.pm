use strict;
use warnings;

use Perl6::MetaModel;

use PIL::Run::Container::Scalar;

sub mk_containers { # consolidate with Array
	map {
		my $c = PIL::Run::Container::Scalar->new_instance();
		$c->scalar_store($_);
		$c;
	} @_;
}

role 'IHash' => {};

class 'PIL::Run::Container::Hash' => {
	does => [ 'IHash' ],
	instance => {
		attrs => [ '%:slots' ],
		BUILD => sub {
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
			_hash_vivifyElem => sub {
				my $self = shift;
				my $idx = shift;
				my $c = $self->hash_fetchElem($idx);
				unless ($c){
					$c = PIL::Run::Container::Scalar->new;
					$self->hash_storeElem($idx, $c);
				}
				$c;
			},
			hash_fetchVal => sub {
				my $self = shift;
				my $idx = shift;
				my $c = $self->hash_fetchElem($idx);
				return undef unless $c;
				$c->scalar_fetch;
			},
			hash_storeVal => sub {
				my $self = shift;
				my $idx = shift;
				my $value = shift;
				$self->_hash_vivifyElem($idx)->scalar_store($value);
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
				# FIXME context should be propagated to methods
				scalar(() = $self->hash_fetchKeys) == 0;
			},
		},
	},
};

1;
