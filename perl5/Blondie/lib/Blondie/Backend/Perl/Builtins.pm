#!/usr/bin/perl

package Blondie::Backend::Perl::Builtins;

use strict;
use warnings;

use Blondie::Prelude;
use Blondie::ADTs;
use Blondie::Nodes;
use Blondie::TypeSafe;

{
    package Blondie::Backend::Perl::Prim;
	use base qw/Blondie::Backend::Prim/;
}

sub native {
    my $params = @_ == 1 ? shift : { @_ };

    my $node = Blondie::Prelude->env->get($params->{name});

    my $digest = $node->digest;

    $digest => Blondie::Backend::Perl::Prim->new(
		equals => $digest,
		body => $params->{body},
		arity => $params->{arity},
		name => $params->{name},
		type => $params->{type},
    );
}

my %p6p5 = (
    '~' => '.',
);

my %by_digest = (
    Blondie::Prelude->env->get('$*OUT')->digest => Val(\*STDOUT),
    map { native($_) } (
        {
            arity => 3,
            name => '&ternary:<?? !!>',
            body => sub {
                $_[1] ? $_[2] : $_[3]
            },
			type => [map { Blondie::TypeSafe::Type::Std->new($_) } qw/GV str/ => "int"], # "bool, x, x -> x"
        },
        {
            arity => 2,
            name => '&print',
            body => sub {
                my $self = shift;
                my $fh = shift->val;
                my $string = shift;
                print $fh $string;
            },
			type => [map { Blondie::TypeSafe::Type::Std->new($_) } qw/GV PV/ => "IV"],
        },
        (
            map {{
				arity => 2,
				name => "&infix:<$_>",
				type => [ map { Blondie::TypeSafe::Type::Std->new($_) } (($_ eq '~') ? "PV" : "IV") x 3 ],
				body => eval 'sub { $_[1] ' . (exists $p6p5{$_} ? $p6p5{$_} : $_) . ' $_[2] }' || die $@,
			}} qw(+ - == <= ~ * ** /), '<',
        ),
    ),
);

sub find {
    my $class = shift;
    my $digest = shift;

    $by_digest{$digest};
}

sub build_indirect_typed_value {
	my $class = shift;
	my $val = shift;
	my $type = shift || $val->type;

	Blondie::TypeSafe::Annotation->new(
		type => $type,
		struct_equiv => Blondie::Val->new(
			Blondie::TypeSafe::Annotation->new(
				type => $type,
				struct_equiv => $val,
			),
		),
	);
}

sub cast {
	my $class = shift;
	my $node = shift;

	my $from = shift;
	my $to = shift;

	Blondie::TypeSafe::Annotation->new(
		type => $to,
		struct_equiv => Blondie::App->new(
			$class->build_indirect_typed_value(
				Blondie::Backend::Perl::Prim->new(
					arity => 3,
					name => '&cast',
					body => \&do_cast,
					type => [ "Type" => "Type" => $to ],
				),
			),
			$node,
			( map { $class->build_indirect_typed_value($_->type, Blondie::TypeSafe::Type::Std->new("Type")) } $from, $to )
		),
	);
}

sub do_cast {
	my $self = shift;
	my $node = shift;
	my $from = shift;
	my $to = shift;

	if ($to eq "PV") {
		return "$node";
	} else {
		die "can't perform runtime cast of ($node)::$from to $to";
	}
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::Perl::Builtins - table of prelude replacement relevant to the perl backend

=head1 SYNOPSIS

    use Blondie::Backend::Perl::Builtins;

=head1 DESCRIPTION

This file bridges between perl and Blondie, by providing implementations of
some math, string and IO operations (including a value for $*OUT).

=cut


