#!/usr/bin/perl

package Blondie::Emitter::Pretty;
use base qw/Blondie::Emitter/;

use strict;
use warnings;

use String::Escape qw/quote printable/;
use Scalar::Util qw/blessed/;

use Blondie::Reducer::DuplicateFinder;
use Blondie::TypeSafe;

sub new {
	my $class = shift;

	bless {
		seen => undef,
	}, $class;
}

sub string {
	my $self = shift;
	my $program = shift;

	my $dup_finder = Blondie::Reducer::DuplicateFinder->new;

	my @nodes = grep { blessed($_) } $dup_finder->duplicate_nodes($program);

	$self->{seen} = {};

	return $self->reduce($program) unless @nodes;

	$self->{seen} = { map { $_ => $_ } @nodes };
	$self->{names} = { $program => '$main' };

	$self->make_name($_) for @nodes;

	my $out = join("\n\n", map {
		my $name = $self->{names}{$_};

		$self->{current_node} = $_;
		"$name = " . $self->reduce($_) . ";\n\n";
	} @nodes, $program);

	$out;
}

sub make_name {
	my $self = shift;
	my $node = shift;

	$self->{names}{$node} ||= '$node_' . ++$self->{node_counter};
}

sub layout {
	my $self = shift;

	our $indent;

	if (@_ > 1 or "@_" =~ /\n/){
	return join(
		"\n" . ("    " x $indent),
		"(", (map { "$_," } @_)
	) . "\n" . ("    " x ($indent-1)) . ")";
	} else {
	return "(@_)";
	}	
}

sub generic_reduce {
	my $self = shift;
	my $node = shift;

	if (exists $self->{seen}{$node}){
		if ($self->{current_node} eq $node) {
			$self->{current_node} = '';
		} else {
			return $self->{names}{$node};
		}
	}

	our $indent ||= 0;
	local $indent = $indent + 1;

	if (Scalar::Util::blessed($node)) {
		if ($node->isa("Blondie::Node")){
			my $reduced = $self->SUPER::generic_reduce($node);
			return $self->constructor($reduced) . $self->layout($reduced->str);
		}
	}

	if (ref $node) {
		use Data::Dumper ();
		local $Data::Dumper::Pad = "    " x ($indent -1);
		local $Data::Dumper::Terse = 1;
		local $Data::Dumper::Indent = 1;
		my $str = Data::Dumper::Dumper($node);
		chomp($str);
		$str =~ s/^\s*//;
		return $str;
	} else {
		return "undef" unless defined $node;
		return "$node" if Blondie::TypeSafe::Annotator->guess_perl_type($node) eq "IV";
		return quote(printable($node)),
	}
}

sub can_reduce { 1 }

sub constructor {
	my $self = shift;
	my $node = shift;

	my $class = ref $node;
	if ($class =~ /^Blondie::(\w+)$/ and Blondie::Nodes->can($1)){
		return $1;
	} else {
		return $class . "->new";
	}
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Emitter::Pretty - An emitter that reduces a program into a string of
(hopefully) perlish text.

=head1 SYNOPSIS

	use Blondie::Emitter::Pretty;

	print Blondie::Emitter::Pretty->new->string($program);

=head1 DESCRIPTION

=cut


