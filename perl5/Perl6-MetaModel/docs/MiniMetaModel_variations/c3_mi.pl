use strict;
use warnings;

our ($Class, $Object);

sub _merge {
	use Tie::RefHash;

	my (@seqs) = @_;
	my @res; 
	while (1) {
		# remove all empty seqences
		my @nonemptyseqs = (map { (@{$_} ? $_ : ()) } @seqs);
		# return the list if we have no more no-empty sequences
		return @res if not @nonemptyseqs; 
		my $cand; # a canidate ..
		foreach my $seq (@nonemptyseqs) {
			$cand = $seq->[0]; # get the head of the list
			my $nothead;            
			foreach my $sub_seq (@nonemptyseqs) {
				# XXX - this is instead of the python "in"
				tie my %in_tail, 'Tie::RefHash', (map { $_ => 1 } @{$sub_seq}[ 1 .. $#{$sub_seq} ]);
				# NOTE:
				# jump out as soon as we find one matching
				# there is no reason not too. However, if 
				# we find one, then just remove the '&& last'
				$nothead++ && last if exists $in_tail{$cand};      
			}
			last unless $nothead; # leave the loop with our canidate ...
			$cand = undef;        # otherwise, reject it ...
		}
		die "Inconsistent hierarchy" if not $cand;
		push @res => $cand;
		# now loop through our non-empties and pop 
		# off the head if it matches our canidate
		foreach my $seq (@nonemptyseqs) {
			shift @{$seq} if $seq->[0] eq $cand;
		}
	}
}

sub _calc {
	my $class = shift;
	use Data::Dumper;
	_merge(
		[ $class ],
		( map { [ _retr($_) ] } @{ ::opaque_instance_attrs($class)->{'$:superclass'} } ),
		[ _superclasses($class) ],
	);
}

sub _superclasses {
	my $class = shift;
	my $super = ::opaque_instance_attrs($class)->{'$:superclass'};
	(ref $super eq 'ARRAY') ? @$super : ($super);
}


my $C3ClassInstance;

sub _retr {
	my $class = shift;

	return () unless defined $class;

	my $mclass = ::opaque_instance_attrs($class)->{'$:class'};

	if (defined($mclass) and $mclass == $C3ClassInstance){
		return @{ ::opaque_instance_attrs($class)->{'@:linearization'} };
	}

	return $class, _retr(_superclasses($class));
}




{
	tests => 2,
	code => sub {
		$C3ClassInstance = $Class->new(
			'$:name'		=> 'c3',
			'$:superclass'	=> $Class,
			'%:methods'		=> {
				'new' => sub {
					my $class = shift;
					my %attrs = (
						'@:linearization' => [],
						@_,
					);

					my @superclasses = @{ $attrs{'$:superclass'} || [] };

					my $c3_class = ::create_opaque_instance(\$class, %attrs);

					::opaque_instance_attrs($c3_class)->{'@:linearization'} = [ _calc($c3_class) ];
					
					$c3_class;
				},	

			},
		);

		my $C3 = $C3ClassInstance->new(
			'$:name'		=> 'c3 class',
			'$:superclass'	=> $Object,
			'%:methods'		=> {
				find_method => sub {
					my $class = shift;
					my $label = shift;

					foreach my $class (@{ ::opaque_instance_attrs($class)->{'@:linearization'} }){
						my $method = ::opaque_instance_attrs($class)->{'%:methods'}{$label};
						return $method if $method;
					}

					return undef;
				},
			},
		);

		my $dA = $C3->new(
			'$:name'		=> 'c3 diamond a',
			'$:superclass'	=> [ $Object ],
			'%:methods'		=> {
				hello => sub { "A::hello" },
			},
		);

		my $idA = $dA->new;
		is($idA->hello, 'A::hello', "hello for instance of A is 'A::hello");

		my $dB = $C3->new(
			'$:name'		=> 'c3 diamond b',
			'$:superclass'	=> [ $dA ],
		);

		my $dC = $C3->new(
			'$:name'		=> 'c3 diamond c',
			'$:superclass'	=> [ $dA ],
			'%:methods'		=> {
				hello => sub { "C::hello" },
			},
		);

		my $dD = $C3->new(
			'$:name'		=> 'c3 diamond d',
			'$:superclass'	=> [ $dB, $dC ],
		);

		my $idD = $dD->new();

		is($idD->hello, "C::hello", "correct ->hello");
	},
}
