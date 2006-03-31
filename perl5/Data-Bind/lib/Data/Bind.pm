package Data::Bind;
use 5.008;
use strict;
our $VERSION = '0.24';

use base 'Exporter';
our @EXPORT = qw(bind_op);
use base 'DynaLoader';
__PACKAGE__->bootstrap;

use Devel::Caller qw(caller_cv caller_args);
use B ();

sub bind_op {
    my %vars = @_;

    my $sig = Data::Bind->sig(map { { var => $_ } } keys %vars);
    $sig->bind({ positional => [ values %vars ],
		 named => {} }, 2);

    # XXX: probably returning the var
    return;
}

sub sig {
    my $class = shift;
    my $now_named = 0;
    my ($named, $positional) = ({}, []);

    for my $param (@_) {
	my $db_param = Data::Bind::Param->new
	    ({ container_var => $param->{var},
	       p5type => substr($param->{var}, 0, 1),
	       name => substr($param->{var}, 1) });

	if ($param->{named}) {
	    $now_named = 1;
	    $named->{$db_param->name} = $db_param;
	    $db_param->is_optional(1)
		unless $param->{required};
	}
	else {
	    Carp::carp "positional argument after named ones" if $now_named;
	    $db_param->is_optional(1)
		if $param->{optional};

	    push @{$positional}, $db_param;
	}
    }

    return Data::Bind::Sig->new
	({ named => $named, positional => $positional});
}

# some higher level stuff

sub _get_cv {
    my $sub = shift;
    return B::svref_2object($sub)->GV->object_2svref;
}

# store sig in the sig slot of the cv's gv
sub sub_signature {
    my $class = shift;
    my $sub = shift;
    my $cv = _get_cv($sub);
    *$cv->{sig} = Data::Bind->sig(@_);
    return;
}

sub arg_bind {
    my $cv = _get_cv(caller_cv(1));
    my @arg = caller_args(1);
    *$cv->{sig}->bind({ positional => $arg[0], named => $arg[1] }, 2);
}

=head1 NAME

Data::Bind - Bind and alias variables

=head1 SYNOPSIS

  use Data::Bind;

  # bind simple variables
  sub foo {
    my $y = 10;
    my $x;

    bind_op('$x' => $y);
  }

  # bind for subroutine calls
  Data::Bind->sub_signature
    (\&formalize,
     { var => '$title' },
     { var => '$subtitle' },
     { var => '$case', named => 1 },
     { var => '$justify', named => 1 });
  sub formalize {
    my ($title, $subtitle, $case, $justify);
    Data::Bind->arg_bind;
  }

=head1 DESCRIPTION

This module implements the semantics for perl6-style variable binding,
as well as subroutine call argument passing and binding, in Perl 5.

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
use Carp qw(croak);


sub bind {
    my ($self, $args, $lv) = @_;
    $lv ||= 1;

    my $pos_arg = $args->{positional};
    for my $param (@{$self->positional || []}) {
	my $current = shift @$pos_arg;
	unless ($current) {
	    last if $param->is_optional;
	    croak "positional argument ".$param->name." is required";
	}
	$param->bind($current, $lv);
    }
    # XXX: report extra incoming positional args

    my $named_arg = $args->{named};
    for my $param_name (keys %{$self->named || {}}) {
	my $param = $self->named->{$param_name};
	if (my $current = $named_arg->{$param_name}) {
	    $param->bind($current, $lv);
	}
	else {
	    croak "named argument ".$param->name." is required"
		unless $param->is_optional;
	}
    }

    # XXX: report extra incoming named args

    return;
}

package Data::Bind::Param;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(name p5type is_optional is_writable is_slurpy container_var subscript));
use Devel::LexAlias qw(lexalias);
use PadWalker qw(peek_my);

sub bind {
    my ($self, $var, $lv) = @_;
    $lv++;
    if ($self->p5type eq '$') {
	lexalias($lv, $self->container_var, $var);
	return;
    }
    my $h = peek_my($lv);
    my $ref = $h->{$self->container_var} or die;
    if ($self->p5type eq '@') {
	if ($self->subscript) {
	    Data::Bind::_av_store($ref, $self->subscript, $var);
	}
	else {
	    @$ref = @$var;
	}
    }
    else {
	die 'not yet';
    }
    return;
}

1;
