
use Perl6::MetaModel;

use P5::PIL::Run::Container::Scalar;

sub mk_containers {
	map {
		my $c = P5::PIL::Run::Container::Scalar;
		$c->scalar_store($_);
		$c;
	} @_;
}

class 'P5::PIL::Run::Container::Array' => {
	does => [ 'IArray' ],
	class => {
		'BUILD' => sub {
			my $self = shift;
			$self->{data} = [];
		}

		'array_fetch' => sub { # evaluated in rvalue contexxt
			my $self = shift;
			@{$self->{data}}
		}

		'array_store' => sub { # evaluated in lvalue context as a list
			my $self;
			@{$self->{data}} = @_;
		}

		'array_fetchKeys' => sub { # returns list of indices
			my $self = shift;
			0 .. $#{$self->{data}};
		}


		'array_fetchElem' => sub { # $idx -> $container # autovivification! - returns **CONTAINER**
			my $self = shift;
			my $idx = shift;
			$self->{data}[$idx]; # type == Container::Scalar
		}
		
		'array_storeElem' => sub { # $idx -> $container -> ()
			my $self = shift;
			my $idx = shift;
			$self->{data}[$idx] = $container;
		}

		'array_fetchVal' => sub { # $idx -> $value
			my $self = shift;
			my $idx = shift;
			$self->array_fetchElem($idx)->scalar_fetch;
		}
 
		'array_storeVal' => sub { # $idx -> $value -> ()
			my $self = shift;
			my $idx = shift;
			my $value = shift;
			$self->array_fetchElem($idx)->scalar_store($value);
		}

		'array_fetchSize' => sub { # $int
			my $self = shift;
			my $idx = shift;
			my $value = shift;
			scalar @{$self->{data}};
		}

		'array_storeSize' => sub { # $int -> ()
			my $self = shift;
			my $size = shift;
			$#{$self->{data}} = ($size - 1);
		}

		'array_extendSize' => sub { # $int -> () # +=?
			my $self = shift;
			my $size  shift;
			$#{$self->{data}} += ($size - 1);
		}

		'array_deleteElem' => sub { # $int -> (); # undef $array[$idx];
			my $self = shift;
			my $idx = shift;
			undef $self->{data}[$idx];
		}

		'array_existsElem' => sub { # $int -> $bool;
			my $self = shift;
			my $idx = shift;
			exists $self->{data}[$idx];
		}

		'array_clear' => sub {
			my $self = shift;
			@{$self->{data}} = (); # just ref = []?
		}

		'array_push' => sub { # @values -> ()
			my $self = shift;
			my $values = shift;
			push @{$self->{data}}, mk_containers(@$values);
		}

		'array_unshift' => sub { # @values -> ()
			my $self = shift;
			my $values = shift;
			unshift @{$self->{data}}, mk_containers(@$values);
		}

		'array_pop' => sub { # $val
			my $self = shift;
			(pop @{$self->{data}})->scalar_fetch;
		}

		'array_shift' => sub { # $val
			my $self = shift;
			(shift @{$self->{data}})->scalar_fetch;
		}

		'array_splice' => sub { # $idx_from -> $idx_to -> @replace_list -> @list
			my $self = shift;
			my $idx_from = shift;
			my $idx_to = shift;
			my $replacement = 	shift;
			map { $_->scalar_fetch } splice(@{$self->{data}}, $idx_from, $idx_to, mk_containers(@$replacement));
		}
	}
}

1;
