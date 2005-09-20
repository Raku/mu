#!/usr/bin/perl

package Blondie::Backend::C::Emitter;
use base qw/Blondie::Emitter Blondie::Reducer::DynamicScoping/;

use strict;
use warnings;

use UNIVERSAL::moniker;
use String::Escape qw/quote printable/;
use Blondie::Reducer::DuplicateFinder;
use Blondie::TypeSafe;
use Set::Object ();

sub new {
	my $class = shift;
	bless {
		defs => [],
		defined_nodes => {},
	}, $class;
}

sub reduce {
	my $self = shift;
	my $node = shift;

	my $h = Devel::STDERR::Indent::indent;
	warn "annotation: $node";
	warn "reducing " . $node->struct_equiv; #. Data::Dumper::Dumper($node->struct_equiv);

	if ($self->{dups}->includes($node->struct_equiv)) {
		$self->add_natural_def($node);
	} else {
		return $self->simple_reduce($node);
	}
}

sub simple_reduce {
	my $self = shift;
	my $node = shift;

	return $self->literal_value($node) unless $self->can_reduce($node);

	if (my $meth = $self->can("reduce_" . $node->struct_equiv->moniker)) {
		return $self->$meth($node);
	} else {
		return $self->generic_reduce($node);
	}

	$self->SUPER::reduce($node);
}

sub literal_value {
	my $self = shift;
	my $node = shift;
	
	my $t = $self->t($node);

	if ($t eq "IV") {
		return $node->struct_equiv;
	} elsif ($t eq "PV") {
		return quote(printable($node->struct_equiv));
	} else { die "don't know to make a literal out of $t" }
}

sub generic_reduce {
	my $self = shift;
	my $node = shift;

	warn "generic reduction over " . $node->struct_equiv;

	my $reduced_struct = $self->SUPER::generic_reduce($node->struct_equiv);
	(ref $node)->new(%$node, struct_equiv => $reduced_struct);
}

sub can_reduce {
	my $self = shift;
	my $node = shift;
	Scalar::Util::blessed($node->struct_equiv);
}

sub t {
	my $self = shift;
	my $node = shift;

	$self->resolve_type($node->type);
}

sub resolve_type {
	my $self = shift;
	my $type = shift;

	$type = $type->type while ref $type;
	$type;
}

sub add_natural_def {
	my $self = shift;
	my $node = shift;

	die "This method is not yet ready";

	my $method;
	if (Scalar::Util::blessed($node->struct_equiv)){
		my $moniker = $node->struct_equiv->moniker;
		$method = "add_${moniker}_def";
	} else {
		$method = "add_const_def";
	}

	$self->$method($self, $node);
}

sub add_const_def {
	my $self = shift;
	my $node = shift;

	my $num = ++$self->{const_counter};
	my $symbol = "const_$num";

	my $type = $self->t($node);
	
	$self->define_node($node, $symbol);

	my $body = $self->simple_reduce($node);

	$self->add_def($node, "const $type $symbol = $body");

}

sub add_thunk_def {
	my $self = shift;
	my $node = shift;
	my $body = shift;

	warn "body $body is blessed" if Scalar::Util::blessed($body);

	warn "adding a definition of $node = " . $body;
	warn "type of $node is " . Data::Dumper::Dumper($node->type);

	my $num = ++$self->{thunk_counter};
	my $symbol = "def_thunk_$num";
	
	my $type = $self->t($node->type->[-1]);

	$self->add_def($node, "$type $symbol $body");
	$self->define_node($node, $symbol);

	$symbol;
}

sub add_def {
	my $self = shift;
	my $node = shift;
	my $string = shift;

	push @{$self->{defs}}, $string;
}


sub define_node {
	my $self = shift;
	my $node = shift;
	my $value = shift;
	$self->{defined_nodes}{$node} = $value;
}

sub reduce_val {
	my $self = shift;
	my $val = shift;

	$self->reduce($val->struct_equiv->val);
}

sub reduce_thunk {
	my $self = shift;
	my $node = shift;

	my $thunk = $node->struct_equiv;
	my $child = $thunk->val->struct_equiv;

	if ($child->isa("Blondie::Seq")){
		my @children = $child->values;
		my @params;
		my @exps;

		$self->enter_scope;
		while(@children) {
			my $sub = shift @children;
			if ($sub->struct_equiv->isa("Blondie::Param")) {
				push @params, $self->resolve_type($sub->accepts_type) ." ". (my $sym = $self->reduce($sub));
				$self->new_pad($sub->struct_equiv->val->struct_equiv => $sym);
			} else {
				push @exps, $self->reduce($sub);
			}
		}
		$self->leave_scope;

		$self->add_thunk_def($node, "(" . join(", ", @params) . ") {\n\t".join(";\n\t", @exps).";\n}");
	} else {
		$self->reduce($thunk->val);
	}
}

sub reduce_sym {
	my $self = shift;
	my $node = shift;

	$self->find_immediate_dyn_sym($node->struct_equiv->val->struct_equiv)->val;
}

sub reduce_param {
	my $self = shift;
	my $node = shift;

	$self->mangle_sym_name($node->struct_equiv->val->struct_equiv);
}

sub mangle_sym_name {
	my $self = shift;
	my $sym = shift;

	require charnames;
	
	$sym =~ s/([^a-z0-9_])/charnames::viacode(ord($1)) . "__"/ge;
	$sym =~ s/\s+/_/g;

	"sym__" . lc($sym);
}

sub reduce_seq {
	my $self = shift;
	my $node = $self->generic_reduce(shift);

	my $seq = $node->struct_equiv;

	join(";\n", $seq->values);
}

sub emit {
	my $self = shift;
	my $prog = shift;

	use Data::Dumper;
	$Data::Dumper::Maxdepth = 7;
	$Data::Dumper::Indent = 1;
	$Data::Dumper::Terse = 1;
	warn Dumper($prog);

	my $dup_finder = Blondie::Reducer::DuplicateFinder->new;
	my $flat = Blondie::TypeSafe::Flatten->new->reduce($prog);
	$self->{dups} = Set::Object->new( $dup_finder->duplicate_nodes($flat) );

	my $main = $self->reduce($prog);

	push @{ $self->{defs} }, "int main () {\n\t(void)$main;\n\treturn 1;\n}";

	join("\n\n", Blondie::Backend::C::Builtins->prelude, @{$self->{defs}});
}

sub reduce_app {
	my $self = shift;
	my $app = shift;

	my $rapp = $self->generic_reduce($app);

	my ($thunk, @params) = $rapp->struct_equiv->values;

	"$thunk( " . join(", ", @params) . " )";
}

sub reduce_prim {
	my $self = shift;
	my $prim = shift;

	$prim->struct_equiv->body;
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::C::Emitter - 

=head1 SYNOPSIS

	use Blondie::Backend::C::Emitter;

=head1 DESCRIPTION

=cut


