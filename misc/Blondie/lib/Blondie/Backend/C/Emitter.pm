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

{
	package Blondie::Backend::C::Emitter::DuplicateFinder;
	use base qw/Blondie::Reducer::DuplicateFinder/;

	sub generic_reduce {
		my $self = shift;
		my $node = shift;

		my $struct = $node->struct_equiv;
		my $orig = $node->orig;

		if (defined($orig) and $self->{seen}->includes($orig)) {
			$self->{dups}->insert($orig);
			return;
		} else {
			if (defined($orig) and not(Scalar::Util::blessed($struct) and $struct->isa("Blondie::Val"))) {
				$self->{seen}->insert($orig);
			}
			$self->Blondie::Reducer::generic_reduce($node->struct_equiv);
		}
	}
}

sub new {
	my $class = shift;
	bless {
		defs => [],
		defined_nodes => Set::Object->new,
		names => {},
	}, $class;
}

sub reduce {
	my $self = shift;
	my $node = shift;

	my $h = Devel::STDERR::Indent::indent;
	warn "annotation: $node";
	warn "reducing " . $node->struct_equiv; #. Data::Dumper::Dumper($node->struct_equiv);

	if ($self->has_symbol($node)){
		return $self->symbolic_representation($node);
	}

	my $symbol = $self->declare($node) if $self->is_duplicate($node);
	warn $node->struct_equiv . " is duplicated, and has been alocated $symbol" if defined $symbol;

	my $result = $self->inner_reduce($node);
	
	if (defined $symbol) {
		return $self->define($node, $symbol, $result);
	} else {
		return $result;
	}
}

sub inner_reduce {
	my $self = shift;
	my $node = shift;
	return $self->literal_value($node) unless Scalar::Util::blessed($node->struct_equiv);

	if (my $meth = $self->can("reduce_" . $node->struct_equiv->moniker)) {
		return $self->$meth($node);
	} else {
		return $self->generic_reduce($node);
	}
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

	$type = $type->type while Scalar::Util::blessed($type);
	$type = $type->[0] while ref $type and @$type == 1;
	$type;
}

sub has_symbol {
	my $self = shift;
	my $node = shift;

	exists $self->{names}{$node->orig};
}

sub declare {
	my $self = shift;
	my $node = shift;

	$self->{names}{$node->orig} ||= do {
		my $struct = $node->struct_equiv;
		my $type = Scalar::Util::blessed($struct) ? $struct->moniker : "const";
		join("_", $type, ++$self->{counters}{$type});
	}
}

sub define {
	my $self = shift;
	my ($node, $symbol, $body) = @_;

	return if $self->node_is_defined($node);

	return $self->define_literal(@_) unless $self->can_reduce($node);

	my $kind = $node->struct_equiv->moniker;
	my $method = "define_$kind";
	warn "defining a new $kind (@_)";
	return $self->$method(@_);
}

sub define_prim {
	my $self = shift;
	my ($node, $symbol, $body) = @_;

	return $body;
}

sub define_literal {
	my $self = shift;
	my ($node, $symbol, $body) = @_;

	my $type = $self->resolve_type($node);
	
	$self->add_definition($node, "const $type $symbol = $body;");

	return $symbol;
}

sub define_thunk {
	my $self = shift;
	my ($node, $symbol, $body) = @_;

	# all thunks with protos were already defined as functions
	
	my $type = $self->resolve_type($node); # unlike parametered thunks these thunk have no -> type

	$self->define_named_block($node, $symbol, $body, $type);
}

sub define_app {
	my $self = shift;
	my ($node, $symbol, $body) = @_;

	$self->define_named_block($node, $symbol, $body, $self->resolve_type($node));
}

sub define_named_block {
	my $self = shift;
	my ($node, $symbol, $body, $type) = @_;
	$self->add_definition($node, "$type $symbol () {\n\t$body;\n}");
	return $symbol . "()";
}

sub define_val {
	my $self = shift;
	my ($node, $symbol, $body, $type) = @_;
	return $body;
}

sub symbolic_representation {
	my $self = shift;
	my $node = shift;

	$self->{names}{$node->orig};
}

sub is_duplicate {
	my $self = shift;
	my $node = shift;

	$self->{dups}->includes($node->orig);
}

sub reduce_val {
	my $self = shift;
	my $val = shift;

	$self->reduce($val->struct_equiv->val);
}

sub add_definition {
	my $self = shift;
	my $node = shift;
	my $body = shift;

	push @{ $self->{defs} }, $body;

	$self->mark_defined($node);
}

sub mark_defined {
	my $self = shift;
	my $node = shift;

	$self->{defined_nodes}->insert($node);
}

sub node_is_defined {
	my $self = shift;
	my $node = shift;
	$self->{defined_nodes}->includes($node);
}

sub reduce_thunk {
	my $self = shift;
	my $node = shift;

	my $thunk = $node->struct_equiv;
	my $child = $thunk->val->struct_equiv;

	if ($child->isa("Blondie::Seq")){ # FIXME if (has_params)
		my $symbol = $self->declare($node);

		my $return_type = $node->type->[-1]->type;
		
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
		
		my $last_exp = pop @exps;
	
		$self->add_definition($node => "$return_type $symbol (" . join(", ", @params) . ") {" . join("\n\t", "", (map { "$_;" } @exps), "return $last_exp;") . "\n}");

		return $symbol;
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
	join("\n\t", "", map { "$_;" } $seq->values);
}

sub emit {
	my $self = shift;
	my $prog = shift;

	use Data::Dumper;
	$Data::Dumper::Maxdepth = 4;
	#$Data::Dumper::Indent = 1;
	$Data::Dumper::Terse = 1;
	#warn Dumper($prog);

	my $dup_finder = Blondie::Backend::C::Emitter::DuplicateFinder->new;
	$self->{dups} = Set::Object->new( $dup_finder->duplicate_nodes($prog) );

	my $main = $self->reduce($prog);
	my $type = $self->resolve_type($prog);

	push @{ $self->{defs} }, "$type b_main () {\n\treturn $main;\n}";
	push @{ $self->{defs} }, "int main () { b_main(); return 0; }";

	join("\n\n", Blondie::Backend::C::Builtins->prelude, @{$self->{defs}});
}

sub reduce_app {
	my $self = shift;
	my $app = shift;

	my $rapp = $self->generic_reduce($app);

	my ($thunk, @params) = $rapp->struct_equiv->values;

	my $orig_thunk = ($app->struct_equiv->values)[0]->struct_equiv->val->struct_equiv; # eep!
	if ($orig_thunk->isa("Blondie::Backend::C::Prim") and ($orig_thunk->fixity || "") eq "infix") {
		return "( $params[0] $thunk $params[1] )";
	} else {
		return "$thunk( " . join(", ", @params) . " )";
	}
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


