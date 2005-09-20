#!/usr/bin/perl

package Blondie::Backend::C;
use base qw/Blondie::Runtime Blondie::Reducer::DynamicScoping/;

use strict;
use warnings;

use UNIVERSAL::require;
use Blondie::TypeSafe ();

use Blondie::Backend::C::Builtins;

sub annotator_class { "Blondie::TypeSafe::Annotator" }

sub annotator {
	my $self = shift;
	#$self->annotator_class->require || die $UNIVERSAL::require::ERROR;
	$self->annotator_class->new;
}

sub annotate {
	my $self = shift;
	$self->annotator->annotate($self, @_);
}

sub emitter_class { "Blondie::Backend::C::Emitter" }

sub emitter {
	my $self = shift;
	$self->emitter_class->require || die $UNIVERSAL::require::ERROR;
	$self->emitter_class->new;
}

sub emit {
	my $self = shift;
	$self->emitter->emit(@_);
}

my %cache;
sub bind {
	my $self = shift;
	my $c_code = shift;

	my $digest = Digest->new("SHA-1");
	$digest->add($c_code);
	my $sha1 = $digest->digest;

	$cache{$sha1} ||= $self->generate_inline_c($c_code);
}

my $i;
sub generate_inline_c {
	my $self = shift;
	my $c_code = shift;

	$c_code =~ s/(.V)/blondie_$1/g;
	$c_code =~ s/int main/int blondie_main/;

	my $func = eval sprintf <<'BIND', __PACKAGE__, ++$i;
		package %s::inline_bindings::%d;
		use Inline C => Config => LIBS => "-lgc"; # http://www.hpl.hp.com/personal/Hans_Boehm/gc/
		Inline->bind(C => $c_code); # marble losing routine
		\&blondie_main;
BIND

	die $@ if $@;

	return $func;
}

sub compile_to_c {
	my $self = shift;
	
	my $compiled = $self->compile(@_);
	warn "compiled: $compiled";
	my $safe = $self->annotate($compiled);

	$self->emit($safe);
}

sub provides {
	my $self = shift;
	my $node = shift;
    Blondie::Backend::C::Builtins->find($node->digest || return);
}

sub cast_node_type {
	my $self = shift;
	my $node = shift;
	my $from = shift->type;
	my $to = shift->type;

	Blondie::TypeSafe::Annotation->new(
		type => $to,
		struct_equiv => Blondie::App->new(
			Blondie::TypeSafe::Annotation->new(
				type => [$from => $to],
				struct_equiv => Blondie::Backend::C::Prim->new(
					arity => 1,
					body => "type_convert_${from}_${to}",
					type => [$from => $to],
				),
			),
			$node,
		),
	);
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::C - 

=head1 SYNOPSIS

	use Blondie::Backend::C;

=head1 DESCRIPTION

=cut


