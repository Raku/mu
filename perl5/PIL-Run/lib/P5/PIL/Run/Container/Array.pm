
use strict;
use warnings;

use Perl6::MetaModel;

use P5::PIL::Run::Container::Scalar;

sub mk_containers {
	map {
		my $c = P5::PIL::Run::Container::Scalar->new;
		$c->scalar_store($_);
		$c;
	} @_;
}

role 'IArray' => {};

class 'P5::PIL::Run::Container::Array' => {
	does => [ 'IArray' ],
	instance => {
		attrs => [ '@:slots' ],
		BUILD => sub {
			my $self = shift;
			$self->set_value('@:slots', []);
		},		
		methods => {
			'array_fetch' => sub { # evaluated in rvalue contexxt
				my $self = shift;
				@{$self->get_value('@:slots')}
			},

			'array_store' => sub { # evaluated in lvalue context as a list
				my $self = shift;
				my $repl = shift;
				@{$self->get_value('@:slots')} = @$repl;
			},

			'array_fetchKeys' => sub { # returns list of indices
				my $self = shift;
				0 .. $#{$self->get_value('@:slots')};
			},

			'array_fetchElem' => sub { # $idx -> $container # autovivification! - returns **CONTAINER**
				my $self = shift;
				my $idx = shift;
				$self->get_value('@:slots')->[$idx]; # type == Container::Scalar
			},
			
			'array_storeElem' => sub { # $idx -> $container -> ()
				my $self = shift;
				my $idx = shift;
				my $container = shift;
				$self->_extend_to_slot($idx);
				$self->get_value('@:slots')->[$idx] = $container;
			},

			'array_fetchVal' => sub { # $idx -> $value
				my $self = shift;
				my $idx = shift;
				$self->array_fetchElem($idx)->scalar_fetch;
			},
	 
			'array_storeVal' => sub { # $idx -> $value -> ()
				my $self = shift;
				my $idx = shift;
				my $value = shift;
				$self->_extend_to_slot($idx);
				$self->array_fetchElem($idx)->scalar_store($value);
			},

			'array_fetchSize' => sub { # $int
				my $self = shift;
				my $idx = shift;
				my $value = shift;
				scalar @{$self->get_value('@:slots')};
			},

			'array_storeSize' => sub { # $int -> ()
				my $self = shift;
				my $size = shift;
				if ($size > (my $orig = $self->array_fetchSize)){
					$self->array_extendSize($size - $orig);
				} else {
					$#{$self->get_value('@:slots')} = ($size - 1);
				}
			},

			'array_extendSize' => sub { # $int -> () # +=?
				my $self = shift;
				my $size = shift;
				push @{$self->get_value('@:slots')}, mk_containers((undef) x ($size));
			},

			'_extend_to_slot' => sub {
				my $self = shift;
				my $idx = shift;
				my $size = $idx + 1;
				if ($self->array_fetchSize < $size){
					$self->array_storeSize($size);
				}
			},

			'array_deleteElem' => sub { # $int -> (); # undef $array[$idx];
				my $self = shift;
				my $idx = shift;
				undef $self->get_value('@:slots')->[$idx];
			},

			'array_existsElem' => sub { # $int -> $bool;
				my $self = shift;
				my $idx = shift;
				exists $self->get_value('@:slots')->[$idx];
			},

			'array_clear' => sub {
				my $self = shift;
				@{$self->get_value('@:slots')} = (); # just ref = []?
			},

			'array_push' => sub { # @values -> ()
				my $self = shift;
				my $values = shift;
				push @{$self->get_value('@:slots')}, mk_containers(@$values);
			},

			'array_unshift' => sub { # @values -> ()
				my $self = shift;
				my $values = shift;
				unshift @{$self->get_value('@:slots')}, mk_containers(@$values);
			},

			'array_pop' => sub { # $val
				my $self = shift;
				(pop @{$self->get_value('@:slots')})->scalar_fetch;
			},

			'array_shift' => sub { # $val
				my $self = shift;
				(shift @{$self->get_value('@:slots')})->scalar_fetch;
			},

			'array_splice' => sub { # $idx_from -> $idx_to -> @replace_list -> @list
				my $self = shift;
				my $idx_from = shift;
				my $idx_to = shift;
				my $replacement = 	shift;
				map { $_->scalar_fetch } splice(@{$self->get_value('@:slots')}, $idx_from, $idx_to, mk_containers(@$replacement));
			},
		},
	},
};

1;

