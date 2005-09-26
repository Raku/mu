#!/usr/bin/perl

package Blondie::Compiler;
use base qw/Blondie::Reducer Blondie::Reducer::DynamicScoping Class::Accessor/;

use Scalar::Util qw/blessed/;

use Blondie::Nodes ();
use Blondie::Emitter::Pretty;

use strict;
use warnings;

BEGIN {
    __PACKAGE__->mk_accessors(qw/runtime cache env/)
}

{
    package Blondie::Compiler::Placeholder;
    use base qw/Blondie::Val/;
    sub set { ${ $_[0] } = $_[1]; bless $_[0], "Blondie::Val" }
}

sub reduce {
    my $self = shift;
    my $node = shift;

    if ($self->can_reduce($node)){
		my $r = $self->runtime;
		if (my $replacement = $r->provides($node)) {
			return $self->reduce($replacement);
		}
	}

    return $self->SUPER::reduce($node);
}

sub new {
    my $class = shift;
    bless {
    }, $class;
}

sub compile {
    my $self = shift;

    $self->runtime(shift);
    $self->env(my $env = shift);

    my $ast = shift;
    $self->cache({});

    $self->reduce($ast);
}

sub reduce_stub {
    my $self = shift;

	my $node = shift;

    die "trying to reduce a stub: " . Blondie::Emitter::Pretty->new->string($node);
}

sub reduce_param {
    my $self = shift;
    my $param = shift;

    $self->new_pad($param->val => undef);

	$self->count_param;

    return $param;
}

sub enter_scope {
	my $self = shift;

	push @{ $self->{param_count_stack} }, 0;
	
	$self->SUPER::enter_scope;	
}

sub leave_scope {
	my $self = shift;

	$self->SUPER::leave_scope;

	pop @{ $self->{param_count_stack} };
}

sub count_param {
	my $self = shift;
	$self->{param_count_stack}[-1]++;
}

sub reduce_app {
    my $self = shift;
    my $app = shift;

	my $sent = scalar($app->values) - 1; # thunk + params

    $self->enter_scope;

    my $reduced = $self->generic_reduce($app);

    my $received = $self->leave_scope;

	die "Thunk is accepting $received parameters, while App is providing $sent parameters: "
		. Blondie::Emitter::Pretty->new->string($reduced)
			if $received != $sent and $reduced->isa("Blondie::Thunk"); # FIXME - enforce param checking on stubs or prims some how?

    return $reduced;
}

sub reduce_sym {
    my $self = shift;
    my $node = shift;
    my $symbol = $node->val;

    # if the symbol is a lexical then it can't be prebound
    eval { $self->find_dyn_sym($symbol) };
    return $node if not $@;

    $self->compile_time_resolution($symbol);
}

sub compile_time_resolution {
    my $self = shift;
    my $symbol = shift;

    return $self->{cache}{$symbol} if exists $self->{cache}{$symbol};

    my $val = $self->{cache}{$symbol} = Blondie::Compiler::Placeholder->new(undef);

    # if the symbol is not a predefined global it's an error. If it exists, it can be reduced to it's value
    my $builtin = $self->env->get($symbol) || die $@;

    # fill the place holder with the compiled version of the builtin
    $val->set($self->reduce($builtin));
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Compiler - the base compiler for Blondie ASTs

=head1 SYNOPSIS

    use Blondie::Compiler;

    my $c = Blondie::Compiler->new;
    $c->compile($runtime, $env, $program);

    # or more conveniently

    my $r = Blondie::Backend::Foo->new;
    my $compiled = $r->compile($program);

=head1 DESCRIPTION

The compiler reduces an AST into an executable program. This process has three
main aspects:

=over 4

=item *

Resolving compile time resolvable symbols into values

=item *

Replacing all values which the runtime provides builtins for with the replacement nodes

=item *

Ensuring that there is no crap in the AST - no calls to stubs and no inexistant symbols

=back

The compiler inherits L<Blondie::Reducer> in order to traverse the node
structure. The only dirty part is C<reduce_sym> which must handle recursive calls.

The compiler inherits L<Blondie::Reducer::DynamicScoping> to assist in the
verification of paramter symbol resolution.

=cut


