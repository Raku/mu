#!/usr/bin/perl

package Blondie::TypeSafe;

use strict;
use warnings;

use Blondie::ADTs;
use Blondie::Nodes;


sub new {
	my $class = shift;
	bless { }, $class;
}

sub typecheck {
	my $self = shift;
	my $runtime = shift;
	my $prog = shift;

	Blondie::TypeSafe::Flatten->new->reduce(
		Blondie::TypeSafe::Annotator->new->annotate($runtime, $prog)
	);
}

{
	package Blondie::TypeSafe::Annotation;
	use base qw/Blondie::Node::Map/;

	use Carp qw/croak/;

	sub fmap {
		my $self = shift;
		die "annotations are not fmappable, flatten first";
	}

	sub values {
		my $self = shift;
		croak "annotations are not eatable, flatten first";
	}

	sub type {
		my $self = shift;
		my $type = $self->SUPER::type;

		# this method always returns the shallowest possible scalar value

		# simplify [["Foo"]] into ["Foo"] 
		#$type = [$type]; # and "Foo" into ["Foo"]
		while (ref $type and ref $type eq 'ARRAY' and @$type == 1){
			$type = $type->[1];
		}

		return $type;
	}

	package Blondie::TypeSafe::Type;
	use base qw/Blondie::Map/;
	
	package Blondie::TypeSafe::Type::Bottom;

	use base qw/Blondie::TypeSafe::Type/;
	sub type { $_[0] }

	package Blondie::TypeSafe::Type::Placeholder;
	use base qw/Blondie::TypeSafe::Type/;

	sub set {
		my $self = shift;
		my $type = shift;

		warn Carp::longmess unless ref $type;

		$self->{type} = $type->type;

		bless $self, "Blondie::TypeSafe::Type::Std";
	}

	package Blondie::TypeSafe::Type::Std;
	use base qw/Blondie::TypeSafe::Type/;

	sub new {
		my $class = shift;
		my $type = shift;

		$class->SUPER::new(type => $type);
	}

}

{
	package Blondie::TypeSafe::Flatten;
	use base qw/Blondie::Reducer/;
	
	use Scalar::Util ();
	
	sub new { bless {}, shift }

	sub reduce {
		my $self = shift;
		my $node = shift;
	
		my $h = Devel::STDERR::Indent::indent();

		warn "flattenning $node " . (eval { $node->struct_equiv } || "(not an object)");

		Scalar::Util::blessed($node) ? $self->SUPER::reduce($node->struct_equiv) : $node;
	}
}

{
	package Blondie::TypeSafe::Annotator;
	use base qw/Blondie::Reducer Blondie::Reducer::DynamicScoping Class::Accessor/;

	use Scalar::Util ();
	use B ();
	use B::Flags ();

	use String::Escape qw/qprintable/;

	use Devel::STDERR::Indent qw/indent $STRING/;
	$STRING = "  ";

	BEGIN { __PACKAGE__->mk_accessors(qw/runtime/) }

	sub annotate {
		my $self = shift;
		my $runtime = shift;
		my $prog = shift;
		
		$self->{runtime} = $runtime;

		$self->reduce($prog);
	}

	sub reduce {
		my $self = shift;
		my $h = indent;
		my $node = shift;
		warn "reducing " . qprintable($node) . "...";
		my $res = $self->SUPER::reduce($node);
		warn "reduce(" . (ref($node) ? $node : qprintable($node)) .") = $res (type = " . $self->display_type($res->type) . ")";
		$res;
	}
	
	sub new {
		my $class = shift;
		bless {}, $class;
	}

	sub generic_reduce {
		my $self = shift;
		my $node = shift;

		if (Scalar::Util::blessed($node) and $node->can("type")){
			return Blondie::TypeSafe::Annotation->new(
				type => $node->type,
				struct_equiv => $node,
				orig => $node,
			);
		} else {
			my $type = $self->guess_perl_type($node);
			warn "type of " . qprintable($node) . " is $type";
			return Blondie::TypeSafe::Annotation->new(
				type => Blondie::TypeSafe::Type::Std->new($type),
				struct_equiv => $node,
				orig => $node,
			);
		}
	}

	sub guess_perl_type {
		my $self = shift;
		my $node = shift;
		
		my $obj = B::svref_2object(ref($node) ? $node : \$node);
		my $class = B::class($obj);

		return $class unless $class eq "PVMG";

		my $flags = $obj->flagspv;

		return "IV" if $flags =~ "IOK";
		return "PV" if $flags =~ "POK";

		die "what type is $node ($flags)?";
	}

	sub reduce_sym {
		my $self = shift;
		my $sym = shift;
		
		my $type = $self->find_dyn_sym($sym->val)->val;

		warn "linking the symbol " . $sym->val . " to $type";

		Blondie::TypeSafe::Annotation->new(
			type => $type,
			orig => $sym,
			struct_equiv => $sym->fmap(sub { Blondie::TypeSafe::Annotation->new(struct_equiv => $_[0], orig => $_[0]) }),
		);
	}

	sub reduce_val {
		my $self = shift;
		my $val = shift;

		my $rval = $val->fmap(sub { $self->reduce($_[0]) });
		my $sub = $rval->val;

		Blondie::TypeSafe::Annotation->new(
			type => $sub->type,
			struct_equiv => $rval,
			orig => $val,
		);
	}

	sub reduce_app {
		my $self = shift;
		my $app = shift;

		my $rapp = $app->fmap(sub { $self->reduce($_[0]) });
		my ($thunk_val, @params) = $rapp->values;

		my $thunk_type = $thunk_val->type;
		my @context = ref $thunk_type eq "ARRAY" ? @$thunk_type : $thunk_type;
		my $return_type = pop @context;

		warn "Unifying paramter types with thunk prototype in $app";

		my $i; my $unified_app = $rapp->fmap(sub {
			my $node = shift;
			return $node unless $i++; # skip the thunk

			my $expected = shift @context;

			#warn "## unifying " . $self->display_type($expected) . " with " . Data::Dumper::Dumper($node);
			$self->unify_type($node, $expected);
		});

		return Blondie::TypeSafe::Annotation->new(
			type => $return_type,
			orig => $app,
			struct_equiv => $unified_app,
		);
	}

	sub simplify_type {
		my $self = shift;
		my $type = shift;
		while (Scalar::Util::blessed($type) and !$type->isa("Blondie::TypeSafe::Type::Placeholder")){
			my $new = $type->type;
			return $type if ref $new and $new == $type;
			$type = $new;
		}
		$type;
	}

	sub display_type {
		my $self = shift;
		my $type = $self->simplify_type(@_);
		Scalar::Util::blessed($type)
			? '<Placeholder>'
			: (ref $type ? ("( " . join(" -> ", map { $self->display_type($_) } @{$type}) . " )") : $type);
	}

	sub unify_type {
		my $self = shift;

		my $node = shift;
		my $expected_type = shift;

		my $de_facto_type = $node->type;
		
		warn "attempting to unify " . $node->struct_equiv . " whose type is " . $self->display_type($de_facto_type) . " with " . $self->display_type($expected_type);
		warn Data::Dumper::Dumper($expected_type) unless defined $self->simplify_type($expected_type);

		return $node if $self->types_eq($de_facto_type, $expected_type);

		warn "types do not eq";

		if ($self->is_placeholder($de_facto_type)) {
			warn "filling " . $self->display_type($expected_type) . " for placeholder of " . Blondie::Emitter::Pretty->new->string(Blondie::TypeSafe::Flatten->new->reduce($node));
			$de_facto_type->set($expected_type);
			return $node;
		} else {
			warn "casting";
			return $self->runtime->cast_node_type($node, $de_facto_type, $expected_type);
		}
	}

	sub is_placeholder {
		my $self = shift;
		my $type = $self->simplify_type(shift);

		Scalar::Util::blessed($type) and $type->isa("Blondie::TypeSafe::Type::Placeholder")
	}

	sub types_eq {
		my $self = shift;
		my $h = indent;

		my $x = shift;
		my $y = shift;

		warn "comparing $x and $y";

		if (Scalar::Util::blessed($x) and Scalar::Util::blessed($y)) {
			die "baaah! expected type cannot be placeholder" if $y->isa("Blondie::TypeSafe::Type::Placeholder");
			if (!$x->isa("Blondie::TypeSafe::Type::Placeholder")) {
				return $self->types_eq($x->type, $y->type);
			} else {
				return undef;
			}
		} elsif (ref($x) and ref($y)) {
			return unless @$x == @$y;
			my @x = @$x;
			my @y = @$y;

			while (@x) {
				return unless $self->types_eq(pop @x, pop @y)
			}

			return 1;
		} elsif (!ref($x) and !ref($y)) {
			return $x eq $y;
		} else {
			return;
		}
	}
	
	sub reduce_thunk {
		my $self = shift;
		my $thunk = shift;

		$self->enter_scope;
		my $rthunk = $thunk->fmap(sub { $self->reduce($_[0]) });
		$self->leave_scope;
		
		my $child = $rthunk->val->struct_equiv;

		if ($child->isa("Blondie::Seq")){
			my @children_of_seq = $child->values; # the children of the seq child of the thunk
			my $last = pop @children_of_seq;

			my @params = grep { $_->struct_equiv->isa("Blondie::Param") } @children_of_seq;
			
			return Blondie::TypeSafe::Annotation->new(
				type => [ ( map { $_->accepts_type } @params ) => $last->type, ],
				struct_equiv => $rthunk,
				orig => $thunk,
			);
		} else {
			return Blondie::TypeSafe::Annotation->new(
				type => [ $child->type ],
				struct_equiv => $rthunk,
				orig => $thunk,
			);
		}
	}

	sub reduce_seq {
		my $self = shift;
		my $seq = shift;
		
		my $rseq = $seq->fmap(sub { $self->reduce($_[0]) });
		
		return Blondie::TypeSafe::Annotation->new(
			type => ($rseq->values)[-1]->type,
			struct_equiv => $rseq,
			orig => $seq,
		);
	}

	sub reduce_param {
		my $self = shift;
		my $param = shift;
		
		$self->new_pad( $param->val, my $placeholder = Blondie::TypeSafe::Type::Placeholder->new );

		my $rparam = $param->fmap(sub {
			Blondie::TypeSafe::Annotation->new(
				struct_equiv => $_[0],
				type => Blondie::TypeSafe::Type::Bottom->new,
				orig => $_[0],
			);
		});

		return Blondie::TypeSafe::Annotation->new(
			type => Blondie::TypeSafe::Type::Bottom->new,
			accepts_type => $placeholder,
			struct_equiv => $rparam,
			orig => $param,
		);
	}

	sub can_reduce {
		my $self = shift;
		my $node = shift;

		1;
	}
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::TypeSafe - 

=head1 SYNOPSIS

	use Blondie::TypeSafe;

=head1 DESCRIPTION

=cut


