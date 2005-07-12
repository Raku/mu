package Perl6::Container::Array;

use strict;
use warnings;

use Perl6::Container::Scalar;

sub mk_containers {
	map {
		my $c = Perl6::Container::Scalar->new;
		$c->scalar_store($_);
		$c;
	} @_;
}

sub new {
	my $pkg = shift;

	my $self = bless {
		data => [],
	}, $pkg;

	$self;
}

sub array_fetch { # evaluated in rvalue contexxt
	my $self = shift;
	$self->{data}; # TODO Perl6::List->new(...containers...)
}

sub array_store { # evaluated in lvalue context as a list
	my $self = shift;
	my $repl = shift;
	@{$self->{data}} = @$repl;
}

sub array_fetchKeys { # returns list of indices
	my $self = shift;
	0 .. $#{$self->array_fetch};
}

sub array_fetchElem { # $idx -> $container # autovivification! - returns **CONTAINER**
	my $self = shift;
	my $idx = shift;
	$self->array_fetch->[$idx]; # type == Container::Scalar
}

sub array_storeElem { # $idx -> $container -> ()
	my $self = shift;
	my $idx = shift;
	my $container = shift;
	$self->_extend_to_slot($idx);
	$self->array_fetch->[$idx] = $container;
}

sub array_fetchVal { # $idx -> $value
	my $self = shift;
	my $idx = shift;
	$self->array_fetchElem($idx)->scalar_fetch;
}

sub array_storeVal { # $idx -> $value -> ()
	my $self = shift;
	my $idx = shift;
	my $value = shift;
	$self->_extend_to_slot($idx);
	$self->array_fetchElem($idx)->scalar_store($value);
}

sub array_fetchSize { # $int
	my $self = shift;
	my $idx = shift;
	my $value = shift;
	scalar @{$self->array_fetch};
}

sub array_storeSize { # $int -> ()
	my $self = shift;
	my $size = shift;
	if ($size > (my $orig = $self->array_fetchSize)){
		$self->array_extendSize($size - $orig);
	} else {
		$#{$self->array_fetch} = ($size - 1);
	}
}

sub array_extendSize { # $int -> () # +=?
	my $self = shift;
	my $size = shift;
	push @{$self->array_fetch}, mk_containers((undef) x ($size));
}

sub _extend_to_slot {
	my $self = shift;
	my $idx = shift;
	my $size = $idx + 1;
	if ($self->array_fetchSize < $size){
		$self->array_storeSize($size);
	}
}

sub array_deleteElem { # $int -> (); # undef $array[$idx];
	my $self = shift;
	my $idx = shift;
	undef $self->array_fetch->[$idx];
}

sub array_existsElem { # $int -> $bool;
	my $self = shift;
	my $idx = shift;
	exists $self->array_fetch->[$idx];
}

sub array_clear {
	my $self = shift;
	@{$self->array_fetch} = (); # just ref = []?
}

sub array_push { # @values -> ()
	my $self = shift;
	my $values = shift;
	push @{$self->array_fetch}, mk_containers(@$values);
}

sub array_unshift { # @values -> ()
	my $self = shift;
	my $values = shift;
	unshift @{$self->array_fetch}, mk_containers(@$values);
}

sub array_pop { # $val
	my $self = shift;
	(pop @{$self->array_fetch})->scalar_fetch;
}

sub array_shift { # $val
	my $self = shift;
	(shift @{$self->array_fetch})->scalar_fetch;
}

sub array_splice { # $idx_from -> $idx_to -> @replace_list -> @list
	my $self = shift;
	my $idx_from = shift;
	my $idx_to = shift;
	my $replacement = 	shift;
	map { $_->scalar_fetch } splice(@{$self->array_fetch}, $idx_from, $idx_to, mk_containers(@$replacement));
}

1;
