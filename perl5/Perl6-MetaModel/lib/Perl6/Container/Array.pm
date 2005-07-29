package Perl6::Container::Array;

use strict;
use warnings;

use Perl6::Container::Scalar;

sub mk_containers {
	map {
		my $c = Perl6::Container::Scalar->new;
		$c->STORE($_);
		$c;
	} @_;
}

sub new {
	my $class = shift;
	return bless { data => [] }, $class;
}

sub FETCH { # evaluated in rvalue contexxt
	(shift)->{data}; # TODO Perl6::List->new(...containers...)
}

sub STORE { # evaluated in lvalue context as a list
	my ($self, $repl) = @_;
	@{$self->{data}} = @{$repl};
}

sub FETCH_KEYS { # returns list of indices
	0 .. $#{(shift)->FETCH};
}

sub FETCH_ELEM { # $idx -> $container # autovivification! - returns **CONTAINER**
	my ($self, $index) = @_;
	$self->FETCH->[$index]; # type == Container::Scalar
}

sub STORE_ELEM { # $idx -> $container -> ()
	my ($self, $index, $container) = @_;
	$self->_extend_to_slot($index);
	$self->FETCH->[$index] = $container;
}

sub FETCH_VAL { # $idx -> $value
	my ($self, $index) = @_;
	$self->FETCH_ELEM($index)->FETCH; # <-- scalar FETCH
}

sub STORE_VAL { # $idx -> $value -> ()
	my ($self, $index, $value) = @_;
	$self->_extend_to_slot($index);
	$self->FETCH_ELEM($index)->STORE($value); # <--- scalar STORE
}

sub FETCH_SIZE { # $int
	scalar @{(shift)->FETCH};
}

sub STORE_SIZE { # $int -> ()
	my ($self, $size) = @_;
	if ($size > (my $orig = $self->FETCH_SIZE)){
		push @{$self->FETCH}, mk_containers((undef) x ($size - $orig));
	} else {
		$#{$self->FETCH} = ($size - 1);
	}
}

sub EXTEND_SIZE { # $int -> () # EXTEND: like storeSize, but never truncate
	my ($self, $size) = @_;
	if ($size > (my $orig = $self->FETCH_SIZE)){
		push @{$self->FETCH}, mk_containers((undef) x ($size - $orig));
	}
}

sub _extend_to_slot {
	my ($self, $index) = @_;
	my $size = $index + 1;
	if ($self->FETCH_SIZE < $size){
		$self->STORE_SIZE($size);
	}
}

sub DELETE_ELEM { # $int -> (); # undef $array[$idx];
	my ($self, $index) = @_;
	undef $self->FETCH->[$index];
}

sub EXISTS_ELEM { # $int -> $bool;
	my ($self, $index) = @_;
	exists $self->FETCH->[$index];
}

sub CLEAR {
	@{(shift)->array_fetch} = (); # just ref = []?
}

sub PUSH { # @values -> ()
	my ($self, $values) = @_;
	push @{$self->FETCH}, mk_containers(@{$values});
}

sub UNSHIFT { # @values -> ()
	my ($self, $values) = @_;
	unshift @{$self->FETCH}, mk_containers(@$values);
}

sub POP { # $val
	(pop @{(shift)->FETCH})->FETCH; # <-- scalar FETCH
}

sub SHIFT { # $val
	(shift @{(shift)->FETCH})->FETCH; # <-- scalar FETCH
}

sub SPLICE { # $idx_from -> $idx_to -> @replace_list -> @list
	my ($self, $index_from, $index_to, $replacement) = @_;
	map { 
	    $_->FETCH # this is scalar FETCH
	} splice(@{$self->FETCH}, $index_from, $index_to, mk_containers(@{$replacement}));
}

1;
