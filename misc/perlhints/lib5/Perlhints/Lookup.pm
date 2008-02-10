package Perlhints::Lookup;

use Carp;
use strict;
use warnings;
use Data::Dumper;

sub new {
	my ($class, $record_ref) = @_;
	if (! ref $record_ref eq 'ARRAY') {
		confess( 'Usage: ' . __PACKAGE__ . '-new($ref_to_records);' );
	}
	my $self = {};
	bless $self, $class;
	$self->{records} = $record_ref;
	return $self;
}

sub lookup {
	my ($self, $lookup) = @_;
	confess("Argument must be at least of length 1") unless length $lookup;
	my $re_lookup = quotemeta $lookup;
	my @results;
	for my $rec (@{$self->{records}}) {
#        print Dumper $rec if !defined $rec->{token};
		if ($lookup eq $rec->{token}) {
			push @results, [1, $rec];
		} elsif ($rec->{token} =~ m/$re_lookup/){
			my $qs = length($lookup) / length($rec->{key});
			push @results, [$qs, $rec];
		} 
	}
	return $self->_sort_and_strip(\@results);
}

sub _sort_and_strip {
	my ($self, $records) = @_;
	return map { $_->[1] }
		   reverse sort {$a->[0] <=> $b->[0]} @{$records};
}

1;
