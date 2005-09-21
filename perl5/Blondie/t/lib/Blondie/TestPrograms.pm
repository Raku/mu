#!/usr/bin/perl

package Blondie::TestPrograms;
use base qw/Exporter/;

use strict;
use warnings;

use Blondie::Nodes;

use Test::Deep;
use Test::Exception;

our @EXPORT = qw/smoke_runtime/;

my @progs = (
	{
		ast => App( Sym('&infix:<+>'), Val(2), Val(2) ),
		result => 4,
		name => "simple addition",
	},
	{
		ast => App(
			Sym('&infix:<+>'),
			App( Sym('&infix:<+>'), Val(1), Val(2) ),
			App( Sym('&infix:<+>'), Val(3), Val(4) ),
		),
		result => 10,
		name => "nested addition",
	},
	{
		ast => App(
			Sym('&infix:<~>'),
			Val('foo'),
			Val('bar'),
		),
		result => 'foobar',
		name => "simple concatenation",
	},
	{
		ast => App(
			Sym('&infix:<*>'),
			Val(2),
			Val(3),
		),
		result => 6,
		name => "simple multiplication",
	},
	{
		ast => App(
			Val(
				Thunk(
					Seq(
						Param('$x'),
						App(
							Sym('&infix:<+>'),
							Sym('$x'),
							Val(2),	
						),
					),
				),
			),
			Val(2),
		),
		result => 4,
		name => "user sub as value",
	},
	{
		ast => App(
			Val(
				Thunk(
					Seq(
						Param('&f'),
						App(
							Sym('&f'),
							Val(2),
						),
					),
				),
			),
			Val(
				Thunk(
					Seq(
						Param('$x'),
						App(
							Sym('&infix:<+>'),
							Val(2),
							Sym('$x'),
						),
					),
				),
			),
		),
		result => 4,
		name => "higher order function",
	},
	{
		ast => App(
			Val(
				Thunk(
					Seq(
						Param('&fact'),
						App(
							Sym('&fact'),
							Val(5),
						),
					),
				),
			),
			Val(
				Thunk(
					Seq(
						Param('$n'),
						App(
							Sym('&control_structure:<if>'),
							App(
								Sym('&infix:<==>'),
								Sym('$n'),
								Val(0),
							),
							Val(
								Thunk(
									Val(1),
								),
							),
							Val(
								Thunk(
									App(
										Sym('&infix:<*>'),
										Sym('$n'),
										App(
											Sym('&fact'),
											App(
												Sym('&infix:<->'),
												Sym('$n'),
												Val(1),
											),
										),
									),
								),
							),
						),
					),
				),
			),
		),
		result => 120,
		name => "recursion via dynamic scope",
	},
);

sub smoke_runtime {
	my $runtime_class = shift;

	foreach my $prog (@progs) {
		my $r = $runtime_class->new;

		my $result;
		
		lives_ok {
			$result = $r->run($prog->{ast});
		} "${runtime_class}->run($prog->{name}) is non-fatal";

		cmp_deeply(
			$result,
			$prog->{result},
			"correct results for $prog->{name}",
		);
	}
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::TestPrograms - 

=head1 SYNOPSIS

	use Blondie::TestPrograms;

=head1 DESCRIPTION

=cut


