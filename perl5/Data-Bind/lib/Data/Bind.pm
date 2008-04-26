package Data::Bind;
use 5.008;
use strict;
our $VERSION = '0.29';

use base 'Exporter';
our @EXPORT = qw(bind_op bind_op2);
use base 'DynaLoader';
__PACKAGE__->bootstrap;

use Devel::Caller qw(caller_cv caller_args);
use B ();

# XXX: Make sig take storage directly
sub bind_op {
    my %vars = @_;

    my $sig = Data::Bind->sig(map { { var => $_, is_rw => 1 } } keys %vars);
    $sig->[0]->bind({ positional => [ values %vars ],
		      named => {} }, 2);

    # XXX: probably returning the var
    return;
}

sub bind_op2 {
    my ($a, $b) = @_;
    if (ref($a) eq 'ARRAY' && ref($b) ne 'ARRAY') {
        # binding @array := $arrayref
	$b = $$b;
    }
    _alias_a_to_b($a, $b, 0);
}

sub sig {
    my ($class, @sigs) = @_;

    if (ref $sigs[0] eq 'HASH') { # one element
	@sigs = ([@sigs]);
    }

    return Data::Bind::SigCollection->new( [ map { $class->sig_element(@$_) } @sigs ] );
}


sub sig_element {
    my $class = shift;
    my $now_named = 0;
    my ($named, $positional, $named_slurpy) = ({}, []);
    my $invocant;
    my $multidim;

    for my $param (@_) {
	die 'more than one multidimensional slurpy argument' if $multidim;
	my $db_param = Data::Bind::Param->new
	    ({ container_var => $param->{var},
	       named_only    => $param->{named_only},
	       is_writable   => $param->{is_rw},
	       is_copy       => $param->{is_copy},
	       is_slurpy     => $param->{is_slurpy},
	       invocant      => $param->{invocant},
	       constraint    => $param->{constraint},
	       p5type        => substr($param->{var}, 0, 1),
	       name          => substr($param->{var}, 1) });

	$multidim = $param->{is_multidimension};
	$db_param->is_slurpy(1) if $multidim;

	if ($param->{invocant}) {
	    $db_param->is_optional(1)
		unless $param->{required};
	    $invocant = $db_param;
	}
	elsif ($param->{named_only}) {
	    if ($db_param->is_slurpy) {
		$named_slurpy = $db_param;
		next;
	    }
	    $now_named = 1;
	    $db_param->is_optional(1)
		unless $param->{required};
	    $named->{$db_param->name} = $db_param;
	}
	else {
	    unless ($db_param->is_slurpy) {
		Carp::carp("positional argument after named ones") if $now_named;
	    }
	    $db_param->is_optional(1)
		if $param->{optional};

	    push @{$positional}, $db_param;
	    $named->{$db_param->name} = $db_param;
	}
    }

    return Data::Bind::Sig->new
	({ named => $named, positional => $positional,
	   is_multidimension => $multidim,
	   invocant => $invocant,
	   named_slurpy => $named_slurpy });
}

# some higher level stuff

sub _get_cv {
    my $sub = shift;
    my $gv = B::svref_2object($sub)->GV;

    if ($gv->SAFENAME eq '__ANON__') {
        # vivify a GV here
        no strict 'refs';
        my $nonce = "__ANON__::$sub";
        return B::svref_2object(\*$nonce)->object_2svref;
    }
    else {
        return $gv->object_2svref;
    }
}

# store sig in the sig slot of the cv's gv
sub sub_signature {
    my $class = shift;
    my $sub = shift;
    my $cv = _get_cv($sub);
    *$cv->{sig} = Data::Bind->sig(@_);
    return $sub;
}

sub arg_bind {
    my $cv = _get_cv(caller_cv(1));
    my @install_local = @{ *$cv->{sig}->bind_all($_[1], 2) };
    # We have to install the locals here, otherwise there can be
    # side-effects when it's too many levels away.
    for (@install_local) {
	my ($name, $code) = @$_;
	no strict 'refs';
	no warnings 'redefine';
	*{$name} = $code;
	Data::Bind::_forget_unlocal(2);
    }
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
     { var => '&code'},
     { var => '$subtitle', optional => 1 },
     { var => '$case', named_only => 1 },
     { var => '$justify', named_only => 1 });
  sub formalize {
    my ($title, $subtitle, $case, $justify);
    Data::Bind->arg_bind(\@_);
  }

  formalize([\('this is title', sub { "some code" }) ], # positional
            { subtitle => \'hello'} ); #named

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

package Data::Bind::SigCollection;
use Data::Capture;

sub new {
    my ($class, $sigs) = @_;
    bless $sigs, $class;
}

use List::Util 'reduce';

sub arity {
    my $self = shift;
    reduce { $a + $b } map { $self->[$_]->arity } 0..$#{$self}
}

sub bind_all {
    my ($self, $arg, $lv) = @_;
    my @install_local;
    ++$lv;
    my $i = 0;
    my $multidim = $self->[0]->is_multidimension;
    my @x;
    while (@$arg) {
	my $inv  = ref($arg->[0]) && ref($arg->[0]) eq 'ARRAY' ? undef : shift @$arg;
	last unless defined $inv || @$arg;
	my $pos = shift @$arg;
	my $named = shift @$arg;

	if ($multidim) {
	    push @x, \Data::Capture->new( { invocant => $inv, positional => $pos, named => $named });
	}
	else {
	    die 'wrong dimension' unless $self->[$i];
	    push @install_local,
		@{ $self->[$i++]->bind({ invocant => $inv, positional => $pos, named => $named }, $lv) };
	}
    }

    if ($multidim) {
	$self->[0]->bind( { positional => \@x }, $lv );
    }

    return \@install_local;
}


sub is_compatible {
    my $self = shift;
    no warnings 'redefine';
    local *Data::Bind::Param::slurpy_bind = sub {};
    local *Data::Bind::Param::bind = sub {};
    local *Data::Bind::Array::bind = sub {};
    local $@;
    eval { $self->bind_all(\@_) };
    return $@ ? 0 : 1;
}

sub bind {
    # XXX: old api
    my $self = shift;
    die 'old api used with multidimension sig' if $#{$self};
    $self->[0]->bind($_[0], $_[1] || 2);
}

package Data::Bind::Sig;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(positional invocant named named_slurpy is_multidimension));
use Carp qw(croak);
use PadWalker qw(peek_my);

sub bind {
    my ($self, $args, $lv) = @_;
    local $Carp::CarpLevel = 2;
    $lv ||= 1;
    my %bound;

    my $pad = peek_my($lv);
    my $named_arg = $args->{named};
    my @ret;

    if ($self->invocant) {
	croak 'invocant missing'
	    if !defined $args->{invocant};

	push @ret, $self->invocant->bind(\$args->{invocant}, $lv, $pad);
    }
    else {
	croak 'unexpected invocant'
	    if defined $args->{invocant};
    }

    for my $param_name (keys %{$self->named || {}}) {
	my $param = $self->named->{$param_name};
	if (my $current = delete $named_arg->{$param_name}) {
	    # XXX: handle array concating
	    push @ret, $param->bind($current, $lv, $pad);
	    $bound{$param_name}++;
	}
	elsif ($param->named_only) {
	    croak "named argument ".$param->name." is required"
		unless $param->is_optional;
	}
    }

    if ($self->named_slurpy) {
	push @ret, $self->named_slurpy->slurpy_bind($named_arg, $lv, $pad);
    }
    else {
	# XXX: report extra incoming named args
    }

    my $pos_arg = $args->{positional};
    for my $param (@{$self->positional || []}) {
	if ($param->is_slurpy && $param->p5type ne '$') {
	    push @ret, $param->slurpy_bind($pos_arg, $lv, $pad);
	    $pos_arg = [];
	    last;
	}
	next if $bound{$param->name};
	my $current = shift @$pos_arg;
	unless ($current) {
	    last if $param->is_optional;
	    croak "positional argument ".$param->name." is required";
	}
	push @ret, $param->bind($current, $lv, $pad);
    }
    # extra incoming positional args
    if (@$pos_arg) {
	croak "extra positional argument.";
    }

    return \@ret;
}


sub arity {
    my $self = shift;
    scalar grep { !$_->is_optional } values %{$self->named};
}

package Data::Bind::Param;
use base 'Class::Accessor::Fast';
__PACKAGE__->mk_accessors(qw(name p5type is_optional is_writable is_copy is_slurpy container_var named_only constraint));
use Devel::LexAlias qw(lexalias);

sub slurpy_bind {
    my ($self, $vars, $lv, $pad) = @_;
    $lv++;

    my $ref = $pad->{$self->container_var} or Carp::confess $self->container_var;

    if ($self->p5type eq '@') {
	my $i = 0;
	# flatten
	for my $var (@$vars) {
	    if (ref($var) eq 'ARRAY') {
		Data::Bind::_av_store($ref, $i++, \$var->[$_]) for 0..$#{$var};
	    }
	    else {
		Data::Bind::_av_store($ref, $i++, $var);
	    }
	}
	return;
    }
    if ($self->p5type eq '%') {
	Data::Bind::_hv_store($ref, $_, $vars->{$_})
	   for keys %$vars;
	return;
    }
    die "not yet";
}

sub bind {
    my ($self, $var, $lv, $pad) = @_;
    $lv++;

	if ( my $constraint = $self->constraint ) {
		unless ( $constraint->(ref $var eq 'SCALAR' ? $$var : $var, level => $lv, pad => $pad, var => $var, param => $self) ) {
			die "Failed constraint of param " . $self->name;
		}
	}

    if ($self->p5type eq '&') {
	return [ (caller($lv-1))[0].'::'.$self->name => $$var ];
    }
    my $ref = $pad->{$self->container_var} or Carp::confess $self->container_var;
    if ($self->p5type eq '$') {
	# XXX: check $var type etc, take additional ref
        if ($self->is_copy) {
            $$ref = $$var;
        }
	elsif ($self->is_writable) {
	    lexalias($lv, $self->container_var, $var);
	}
	else {
	    if (ref($var) eq 'ARRAY' || ref($var) eq 'HASH') {
		Data::Bind::_alias_a_to_b($ref, \$var, 1);
	    }
	    elsif (defined $$var) {
		Data::Bind::_alias_a_to_b($ref, $var, 1);
	    }
	}
	return;
    }
    if ($self->p5type eq '@') {
	Data::Bind::_alias_a_to_b($ref, $var, !$self->is_writable);
    }
    else {
	die 'not yet';
    }
    return;
}

package Data::Bind::Array;
use base 'Tie::Array';

sub FETCH {
  $_[0]->{real}->[$_[1]];
}

sub STORE {
  $_[0]->{real}->[$_[1]] = $_[2];
}

sub FETCHSIZE {
  scalar @{$_[0]->{real}};
}

1;

=head1 SEE ALSO

L<Sub::Multi>

B<TODO: > Add a good reference to Perl6 multiple dispatch here. 

B<TODO: > Add a good reference to Perl6 variable binding semantics

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2006 by Chia-liang Kao and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
