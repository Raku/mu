package Data::Bind;
use 5.008;
use strict;
our $VERSION = '0.24';

use base 'Exporter';
our @EXPORT = qw(bind_op);

sub bind_op {
    my %vars = @_;
    my $sig = Data::Bind::Sig->new
	({ positional =>
	   [ map { Data::Bind::Param->new({ container_var => $_ }) } keys %vars ] });
    $sig->bind({ positional => [map { Data::Bind::Arg->new({ container => $_ }) } values %vars ],
		 named => {} }, 2);

    # XXX: probably returning the var
    return;
}

=head1 NAME

Data::Bind - Bind and alias variables

=head1 SYNOPSIS

  use Data::Bind;

  sub foo {
    my $y = 10;
    my $x;

    bind_op('$x' => $y);
  }

=head1 DESCRIPTION

This is to implement the semantics for perl6-style argument passing
and binding, in Perl 5.

=head1 AUTHORS

Chia-liang Kao <clkao@clkao.org>

=head1 COPYRIGHT

Copyright (c) 2006. Chia-liang Kao. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

package Data::Bind::Sig;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(positional named));
use Devel::LexAlias qw(lexalias);

sub bind {
    my ($self, $args, $lv) = @_;
    $lv ||= 1;
    my $pos_arg = $args->{positional};
    for my $param (@{$self->positional || []}) {
	my $current = shift @$pos_arg or die;
	lexalias($lv, $param->container_var, $current->container);
    }
    my $named_arg = $args->{named};
    for my $param_name (keys %{$self->named || {}}) {
	my $param = $self->named->{$param_name};
	if (my $current = $named_arg->{$param_name}) {
	    lexalias($lv, $param->container_var, $current->container);
	}
    }
    return;
}

package Data::Bind::Arg;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(container name));

package Data::Bind::Param;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(name is_optional is_writable is_slurpy container_var));

1;
