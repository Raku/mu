#!/usr/bin/perl -CDS

# A typed lambda calculus in perl6-ish perl 5
# (makes use of attribute grammars and multimethods)

# first part is a type pretty printer and the type object model
# it emits greek letters and unicode symbols, or type literals

# second part is the object model for a lambda calculus, and a type inferrencer

# the algorithm implemented is Hindley-Milner type inferrencing, as explained in
# this article: http://research.microsoft.com/Users/luca/Papers/BasicTypechecking.pdf

# please note that this is nothingmuch's first article that he read from cover
# to cover with no hand holding, and actually understood. Everybody cheer!
# (i did not understand (or try very hard to understand) the modula 2 code, so
# the impl is probably different)


use utf8;
use strict;
use warnings;

use Set::Object ();
use Data::Dumper ();
use Storable ();

package type;

### the types of types we have are:
# type::operator
#	::binary - takes 2 params (e.g. ->, x)
#	::nullary - takes 0 params (e.g. int)
# type::variable - a placeholder to make a DAG out of a tree (to gain strong typing)
# 	::free - an unbound type variable
#   ::instantiated - points to another variable or operator

use Language::AttributeGrammar;

# this attribute grammer pretty prints bare types (no foralll)
my $pretty = Language::AttributeGrammar->new({ prefix => 'type::' }, <<'#\'EOG');
#\
variable::instantiated: layout($$) = { $.instance }
operator::nullary: layout($$) = { $.name }
operator::binary: layout($$) = { join(" ", layout($.left), $.symbol, layout($.right)) }
#'EOG

sub stringify {
	my $dag = shift;

	my @free;

	my $tree = $dag->splat(\@free); # Attribute grammars can't eat DAGs, so we have to recursively make shallow copies of the nodes
	# \@free will contain all the free type variables in the type at the end of the pass, and the original object they had
	# this lets us assign letters to them

	my @letters = split //, "αβγδεζηθικλμνξοπρστυφχψω"; # some identifiers for types. Let'shope we never need to print more than these ;-)
	
	my %map;
    my @forall;

	# allocate letters for type variables
	# it's ok to destroy the objects since they're copies
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

sub free { 0 } # most types are not free variables

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

sub new {
	my $class_that_should_be = shift;
	my $class = __PACKAGE__;
	my $self = $class->SUPER::new;

	$self->{left} = shift;
	$self->{right} = shift;
	
	# FIXME - this is a hack for L::AG, since it doesn't know about @ISA
	$self->{symbol} = $class_that_should_be->symbol;
	$self->{class_that_should_be} = $class_that_should_be;

	$self;
}


sub isa { # FIXME this is the same hack for L::AG
	my $self = shift;
	my $class = shift;

	if (Scalar::Util::blessed($self)){
		return 1 if $self->{class_that_should_be}->isa($class);
	}
	 
	$self->SUPER::isa($class);
}

sub splat { # recursive splatting happens in parametrized type operators, since they have children
	my $self = shift;
	my $accum = shift;

	bless {
		%$self,	
		left => $self->{left}->splat($accum),
		right => $self->{right}->splat($accum),
	}, (ref $self);
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

# a free variable is a placeholder
# when going into an AST these are generated
# as the AST is left they are unified
# a free variable can be instantiated to point to another type object
# at this point it reblesses itself

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

sub free { 1 } # whether or not this is a free variable

package type::variable::instantiated;
use base qw/type::variable/;

sub free {
	my $self = shift;
	$self->{instance}->free; # a unified variable might be free if it's instantiated to a free var
}

sub splat { # this simplifies the AG above, and avoids unnecessary duplication
	my $self = shift;
	$self->{instance}->splat(@_);
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


# The unification of the various types depends on their data...
# this is why this code is so big.

# in a language with native multimetods this would be much more concise

# a free variable instantiates into whatever it is combined with (except instantiated variables, which delegate. see below)
multi unify => ( 'type::variable::free', any(qw/type::variable::free type::operator/) ) => sub { $_[0]->instantiate($_[1]) };
multi unify => ('type::operator', 'type::variable::free') => sub { $_[1]->instantiate($_[0]) };

# an instantiated variable will unify it's instantiated value instead of itself
multi unify => ('type::variable::instantiated', 'type') => sub { unify($_[0]{instance}, $_[1], @_[2, 3]) };
multi unify => ('type', 'type::variable::instantiated') => sub { unify($_[1]{instance}, $_[0], @_[2, 3]) };
multi unify => ('type::variable::instantiated', 'type::variable::instantiated') => sub { unify($_[0]{instance}, $_[1]{instance}, @_[2, 3]) };

# nullary operators much have the same name (e.g. int == int, str == str, int != str)
multi unify => ('type::operator::nullary', 'type::operator::nullary') => sub {
	my ($x, $y) = (shift, shift);

	$x->{name} eq $y->{name}
		or die "can't unify $x->{name} with $y->{name} in " . Dumper(@_);
};

# binary operators must have the same symbol, and are also unified recursively
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


# these are the type signatures of some builtins
our %prelude = (
	# int -> int -> int
	add => type::operator::arrow->new(
		type::operator::nullary->new("int"),	
		type::operator::arrow->new(
			type::operator::nullary->new("int"),
			type::operator::nullary->new("int"),
		),
	),
	# a -> b -> (a x b)
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
	# (a x b) -> a
	fst => do {
		my $a = type::variable::free->new;
		my $b = type::variable::free->new;
		
		type::operator::arrow->new(
			type::operator::pair->new( $a, $b ),
			$a,
		);
	},
	# (a x b) -> b
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
use Test::More 'no_plan';
use Test::Exception;
use Data::Dumper;
use Language::AttributeGrammar;

$Data::Dumper::Terse = 1; # no need for pointless $VARx


sub AUTOLOAD { # an easy constructor... fun() instead of fun->new()
	(my $class) = ((our $AUTOLOAD) =~ /([^:]+) $/x);
	$class->new(@_);
}


# this is the actual inferrence engine
my $i = Language::AttributeGrammar->new(<<'#\'EOG');
#\

# at the root we initialize the predefined symbols, and the (empty) set of generic variables
ROOT: env($$) = { Storable::dclone(\%types::prelude) }
ROOT: generic_in($$) = { Set::Object->new }

# entering a function creates a new env (the parameter is associated with a new type variable)
# this new env is passed down to the body, but is also reused in the 'type' attr
fun: env($.body) = { new_env($$) }
fun: new_env($$) = {
	my $orig = env($$);
	my %new = (
		%$orig,
		$.param => type::variable::free->new()
	);
	\%new;
}

# inherit the set of generic vars to our body
fun: generic_in($.body) = { generic_in($$) }

# possibly amend the list of generic variables if the paramter type of the function is free
fun: generic($$) = {
	my $t_of_param = type($$)->{left};
	my $prev_generic = generic($.body);

	if ($t_of_param->free) {
		return $prev_generic + Set::Object->new( $t_of_param )
	}

	$prev_generic;	
}

# the type of the function the arrow operator, applied to the type of the parameter (as allocated in the new env), and the type of the body
fun: type($$) = {
	type::operator::arrow->new(
		new_env($$)->{ $.param },
		type($.body),
	),
}

# identifiers pass the generic variables unchanged
ide: generic($$) = { generic_in($$) }

# if an identifier is not in the lexical scope we can't know it's type
ide: type($$) = {
	my $t = env($$)->{$.v};

	die "no such identifier " . $.v unless defined $t;
	
	$t;
}

# values also pass the set of generics unchanged
val: generic($$) = { generic_in($$) }
# vals are assumed to know their own type (see package val above)
val: type($$) = { $.t }

# combs (applications) pass their env to eval their children
comb: env($.fun) = { env($$) }
comb: env($.param) = { env($$) }

# combs combine the generic values from their children
comb: generic_in($.fun) = { generic_in($$) }
comb: generic_in($.param) = { generic_in($$) }
comb: generic($$) = { generic($.fun) + generic($.param) }

comb: type($$) = {
	my $f = type($.fun);
	my $p = type($.param);

	# first of all the type of the thing we're appling must be a function
	die "Can't apply non function type (" . $.fun . " has type " . $f->stringify
		unless $f->isa("type::operator::arrow");
	
	my $p_f = $f->{left};
	my $ret = $f->{right};
	
	my $generic_vars = generic($.fun);

	# if the function is generic we'll be instantiating it's type variables in the function body
	# for this reason the function needs to be cloned (so it will stay generic when we leave the body of the function)
	if ($generic_vars->includes($p_f)) {
		($p_f, $ret) = @{ Storable::dclone([ $p_f, $ret ]) }; # keep $ret bound to $p_f
	}

	# now we unify the type of the parameter with the type to the left of the arrow in the function's type,
	# this causes the type variables in the cloned function to be instantiated to the concrete values as inferred in the body of the function, based on the param
	types::unify(
		$p, $p_f,
		$.fun, $.param,
	);

	# the value of the comb is the return value from the function
	# since this return value is a type variable that is present inside the function, if it was free it might be unified to a concrete value
	$ret;
}

# in a let block we thread the set of generic type variables, so the body has the generic variables created in the binding
let: generic_in($.is) = { generic_in($$) }
let: generic_in($.in) = { generic($.is) }
let: generic($$) = { generic($.in) }

# the env is augmented to contain the new symbol introduced by the binding
# the body of the binding contains the env with the the symbols of our outer scope
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


# these are some sample ASTs that need to be typed
my $plus_ten = fun(
	param => "x",
	body => comb(
		fun => comb(
			fun => ide("add"),
			param => val(10),
		),
		param => ide("x"),	
	),
);

my $id = fun(
	param => "x",
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
	param => "x",
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



# tests for the type pretty printer
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


# tests for the type inferrencer
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

# test that a generic function is flattenned within the body of a function
dies_ok { t($bad_use_of_id) } "can't type λf.(pair (f 3))(f 'foo')";

# a helper that returns the stringified type of an AST
sub t {
	my $ast = shift;
	$i->apply($ast)->type->stringify;
}


