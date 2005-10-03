#!/usr/bin/perl -CDS

use utf8;
use strict;
use warnings;

use Set::Object ();
use Data::Dumper ();
use Storable ();

use lib '/Users/nothingmuch/Perl/luqui/Language-AttributeGrammar/lib';

package type;
use Language::AttributeGrammar;

my $pretty = Language::AttributeGrammar->new({ prefix => 'type::' }, <<'#\'EOG');
#\

variable::instantiated: layout($$) = { $.instance }

operator::nullary: layout($$) = { $.name }

operator::binary: layout($$) = { join(" ", layout($.left), $.symbol, layout($.right)) }

#'EOG

sub stringify {
	my $dag = shift;

	my @free;

	my $tree = $dag->splat(\@free);

	my @letters = split //, "αβγδεζηθικλμνξοπρστυφχψω";
	
	my %map;
    my @forall;

	foreach my $entry (@free) {
		my $assigned = $map{$entry->{orig}};
		unless ($assigned) {
			$assigned = shift @letters;
			$map{$entry->{orig}} = $assigned;
			push @forall, $assigned;
		}
		$entry->{new}->instantiate($assigned);
	}

	eval {
	   join(" ",
	       (map { "∀${_}." } @forall),
	       $pretty->apply($tree)->layout,
	   );
    } || die "$@\n" . Data::Dumper::Dumper($dag);
}

sub splat {
	my $self = shift;
	my $accum = shift;

	$self->shallow_clone;
}

sub shallow_clone {
	my $self = shift;
	bless {%$self}, (ref $self);
}

sub free { 0 }

package type::operator;
use base qw/type/;

sub new {
	my $class = shift;
	bless {}, $class;
}

package type::operator::nullary;
use base qw/type::operator/;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new;
    $self->{name} = shift;
    $self;
}

package type::operator::binary;
use base qw/type::operator/;

use Scalar::Util ();

sub isa {
	my $self = shift;
	my $class = shift;

	if (Scalar::Util::blessed($self)){
		return 1 if $self->{class_that_should_be}->isa($class);
	}
	 
	$self->SUPER::isa($class);
}

sub splat {
	my $self = shift;
	my $accum = shift;

	bless {
		%$self,	
		left => $self->{left}->splat($accum),
		right => $self->{right}->splat($accum),
	}, (ref $self);
}

sub new {
	my $class_that_should_be = shift;
	my $class = __PACKAGE__;
	my $self = $class->SUPER::new;

	$self->{left} = shift;
	$self->{right} = shift;
	
	$self->{symbol} = $class_that_should_be->symbol; # FIXME - does L::AG get along with @ISA?

	$self->{class_that_should_be} = $class_that_should_be;

	$self;
}

package type::operator::arrow;
use base qw/type::operator::binary/;

sub symbol { "→" }

package type::operator::pair;
use base qw/type::operator::binary/;

sub symbol { "×" }

package type::variable;
use base qw/type/;

package type::variable::free;
use base qw/type::variable/;

sub new {
	my $class = shift;
	bless {
		instance => undef,
	}, $class;
}

sub instantiate {
	my $self = shift;
	$self->{instance} = shift;
	bless $self, "type::variable::instantiated";
}

sub splat {
	my $self = shift;
	my $accum = shift;

	push @$accum, my $entry = {
		orig => $self,
		new => undef,
	};
	
	$entry->{new} = $self->SUPER::splat($accum);
}

sub free { 1 }

package type::variable::instantiated;
use base qw/type::variable/;

sub free {
	my $self = shift;
	$self->{instance}->free;
}

sub splat {
	my $self = shift;
	$self->{instance}->splat(@_);
}

package main;
use Test::More 'no_plan';
use Test::Exception;

{
	my $type = type::operator::arrow->new(
		type::variable::free->new,
		my $b = type::variable::free->new,
	);

	is($type->stringify, "∀α. ∀β. α → β", "pretty print a -> b");

	$b->instantiate( type::operator::nullary->new("int") );

	is($type->stringify, "∀α. α → int", "pretty print a -> b where b = int");
}


{
    my $a = type::variable::free->new;
    my $type = type::operator::arrow->new($a, $a);

	is($type->stringify, "∀α. α → α", "pretty print a -> a");
}


{
	my $type = type::operator::arrow->new(
		my $a = type::variable::free->new,
		my $b = type::variable::free->new,
	);

	$a->instantiate($b);

	is($type->stringify, "∀α. α → α", "pretty print a -> b where a = b");
}
















package node;

sub new {
	my $class = shift;
	bless {
		t => undef,
		(@_ == 1 ? (v => $_[0]) : @_),
	}, $class;
}

package fun;
use base qw/node/;

package comb;
use base qw/node/;

package ide;
use base qw/node/;

package let;
use base qw/node/;

package val;
use base qw/node/;

sub new {
	my $self = shift->SUPER::new(@_);
	$self->{t} ||= type::operator::nullary->new("int");
	$self;
}


package types;

use Class::Multimethods::Pure;
use Data::Dumper;

multi unify => ( 'type::variable::free', any(qw/type::variable::free type::operator/) ) => sub { $_[0]->instantiate($_[1]) };
multi unify => ('type::operator', 'type::variable::free') => sub { $_[1]->instantiate($_[0]) };

multi unify => ('type::variable::instantiated', 'type') => sub { unify($_[0]{instance}, $_[1], @_[2, 3]) };
multi unify => ('type', 'type::variable::instantiated') => sub { unify($_[1]{instance}, $_[0], @_[2, 3]) };
multi unify => ('type::variable::instantiated', 'type::variable::instantiated') => sub { unify($_[0]{instance}, $_[1]{instance}, @_[2, 3]) };

multi unify => ('type::operator::nullary', 'type::operator::nullary') => sub {
	my ($x, $y) = (shift, shift);

	$x->{name} eq $y->{name}
		or die "can't unify $x->{name} with $y->{name} in " . Dumper(@_);
};

multi unify => ('type::operator::binary', 'type::operator::binary') => sub {
	my ($x, $y) = (shift, shift);
	
	$x->{symbol} eq $y->{symbol}
		or die "Can't unify $x->{symbol} with $y->{symbol} in " . Dumper(@_);
	
	unify($x->{left}, $y->{left}, @_);
	unify($x->{right}, $y->{right}, @_);
};

multi unify => ('type', 'type') => sub {
	die "trying to unify unknown type type: " . Dumper(@_);
};

our %prelude = (
	add => type::operator::arrow->new(
		type::operator::nullary->new("int"),	
		type::operator::arrow->new(
			type::operator::nullary->new("int"),
			type::operator::nullary->new("int"),
		),
	),
	pair => do {
		my $a = type::variable::free->new;
		my $b = type::variable::free->new;
		
		type::operator::arrow->new(
			$a,
			type::operator::arrow->new(
				$b,
				type::operator::pair->new( $a, $b ),
			),
		);
	},
	fst => do {
		my $a = type::variable::free->new;
		my $b = type::variable::free->new;
		
		type::operator::arrow->new(
			type::operator::pair->new( $a, $b ),
			$a,
		);
	},
	snd => do {
		my $a = type::variable::free->new;
		my $b = type::variable::free->new;
		
		type::operator::arrow->new(
			type::operator::pair->new( $a, $b ),
			$b,
		);
	},
);

package main;

sub AUTOLOAD {
	(my $class) = ((our $AUTOLOAD) =~ /([^:]+) $/x);
	$class->new(@_);
}

use Data::Dumper;

$Data::Dumper::Terse = 1;


use Language::AttributeGrammar;

my $i = Language::AttributeGrammar->new(<<'#\'EOG');
#\

ROOT: env($$) = { Storable::dclone(\%types::prelude) }
ROOT: generic_in($$) = { Set::Object->new }


fun: env($.body) = { new_env($$) }

fun: new_env($$) = {
	my $orig = env($$);
	my %new = (
		%$orig,
		($.param)->{v} => type::variable::free->new()
	);
	\%new;
}

fun: generic_in($.body) = { generic_in($$) }

fun: generic($$) = {
	my $t_of_param = type($$)->{left};
	my $prev_generic = generic($.body);

	#warn "constructing new generic type? $t_of_param ammends @{ $prev_generic } if free (" . $t_of_param->free . ")";

	return $prev_generic + Set::Object->new( $t_of_param )
		if $t_of_param->free;

	$prev_generic;	
}


fun: type($$) = {
	type::operator::arrow->new(
		new_env($$)->{ ($.param)->{v} },
		type($.body),
	),
}

ide: generic($$) = { generic_in($$) }
ide: type($$) = {
	my $t = env($$)->{$.v};

	die "no such identifier " . $.v
		unless defined $t;
	
	$t;
}

val: generic($$) = { generic_in($$) }
val: type($$) = { $.t }

comb: env($.fun) = { env($$) }
comb: env($.param) = { env($$) }

comb: generic_in($.fun) = { generic_in($$) }
comb: generic_in($.param) = { generic_in($$) }
comb: generic($$) = { generic($.fun) + generic($.param) }

comb: type($$) = {
	my $f = type($.fun);
	my $p = type($.param);

	die "Can't apply non function type (" . $.fun . " has type " . $f->stringify
		unless $f->isa("type::operator::arrow");
	
	my $p_f = $f->{left};
	my $ret = $f->{right};
	
	my $generic_vars = generic($.fun);

#	warn "is $p_f or $p or $ret or $f in (@{ $generic_vars }) ?";

	if ($generic_vars->includes($p_f)) {
		($p_f, $ret) = @{ Storable::dclone([ $p_f, $ret ]) }; # keep $ret bound to $p_f
	}

	types::unify(
		$p, $p_f,
		$.fun, $.param,
	);

	$ret;
}

let: generic_in($.is) = { generic_in($$) }
let: generic_in($.in) = { generic($.is) }
let: generic($$) = { generic($.in) }

let: env($.is) = { env($$) }
let: env($.in) = {
	my $orig = env($$);
	my %new = (
		%$orig,
		($.ide) => type($.is),
	);
	\%new;
}

let: type($$) = { type($.in) }

#'EOG

my $plus_ten = fun(
	param => ide("x"),
	body => comb(
		fun => comb(
			fun => ide("add"),
			param => val(10),
		),
		param => ide("x"),	
	),
);

my $id = fun(
	param => ide("x"),
	body => ide("x"),
);

my $fifteen = comb(
	fun => $plus_ten,
	param => val(5),
);

my $half_pair = comb(
	fun => ide("pair"),
	param => val(10),
);

my $pair = comb(
	fun => $half_pair,
	param => val(v => "foo", t => type::operator::nullary->new("str")),
);

my $snd = comb(
	fun => ide("snd"),
	param => $pair,
);

my $fst = comb(
	fun => ide("fst"),
	param => $pair,
);

my $id_pair = let(
	ide => "id",
	is => $id,
	in => comb(
		fun => ide("id"),
		param => comb(
			fun => comb (
				fun => ide("pair"),
				param => comb(
					fun => ide("id"),
					param => val(10),
				),
			),
			param => comb(
				fun => ide("id"),
				param => val(v => "foo", t => type::operator::nullary->new("str")),
			),
		),
	),
);

my $bad_use_of_id = fun(
	param => ide("f"),
	body => comb(
		fun => comb(
			fun => ide("pair"),
			param => comb(
				fun => ide("f"),
				param => val(3),
			),
		),
		param => comb(
			fun => ide("f"),
			param => val(v => "foo", t => type::operator::nullary->new("str")),
		),
	),
);

my $app_id = let(
	ide => "id",
	is => $id,
	in => comb(
		fun => ide("id"),
		param => val(10),
	),
);

is(t($id), "∀α. α → α", ":t id");
is(t($app_id), "int", ":t id(3)");
is(t($plus_ten), "int → int", ":t (+ 10)");
is(t($fifteen), "int", ":t (+ 10) 5");
is(t($half_pair), "∀α. α → int × α", ":t pair(10)");
is(t($pair), "int × str", ":t pair(10)('foo')");
is(t($fst), "int", ":t fst( pair(10)('foo') )");
is(t($snd), "str", ":t snd( pair(10)('foo') )");
is(t(ide("pair")), "∀α. ∀β. α → β → α × β", ":t pair");
is(t(ide("fst")), "∀α. ∀β. α × β → α", ":t fst");
is(t(ide("snd")), "∀α. ∀β. α × β → β", ":t snd");
is(t($id_pair), "int × str", ":t id(pair(id(10))(id('foo')))");

dies_ok { t($bad_use_of_id) } "can't type λf.(pair (f 3))(f 'foo')";


sub t {
	my $ast = shift;
	$i->apply($ast)->type->stringify;
}


