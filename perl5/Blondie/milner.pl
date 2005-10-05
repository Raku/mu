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

use Devel::STDERR::Indent;

package type;
use base qw/Clone/;

### the types of types we have are:
# type::operator
#	::binary - takes 2 params (e.g. ->, x)
#	::nullary - takes 0 params (e.g. int)
# type::variable - a placeholder to make a DAG out of a tree (to gain strong typing)
#	::free - an unbound type variable
#	::instantiated - points to another variable or operator

use Language::AttributeGrammar;

# this attribute grammer pretty prints bare types (no foralll)
my $pretty = Language::AttributeGrammar->new(<<'#\'EOG');
#\
type::variable::instantiated: layout($$) = { $.instance }
type::operator::nullary: layout($$) = { $.name }

type::operator::nary: layout($$) = { join(" ", $.name, layout($.param)) }
type::operator::nary: level($.param) = { level($$) }

ROOT: level($$) = { 0 }
type::operator::binary: level($.left) = { level($$) + 1 }
type::operator::binary: level($.right) = { level($$) }

type::operator::binary: inner_layout($$) = { join(" ", layout($.left), $.symbol, layout($.right)) }
type::operator::binary: layout($$) = { return parenthesize($$) ? "($_)" : $_ for inner_layout($$) }

type::operator::binary: parenthesize($$) = { $$->isa("type::operator::pair") || level($$) }
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

sub is_arrow { 0 }; # or arrows
sub free_vars { () }; # typicall there are no free vars inside a thingy

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

sub left { $_[0]{left} }
sub right { $_[0]{right} }


sub isa { # FIXME this is the same hack for L::AG
	my $self = shift;
	my $class = shift;

	if (Scalar::Util::blessed($self)){
		return 1 if $self->{class_that_should_be}->isa($class);
	}
	 
	$self->SUPER::isa($class);
}

sub is_arrow {
	my $self = shift;
	$self->{class_that_should_be}->is_arrow;
}

sub splat { # recursive splatting happens in parametrized type operators, since they have children
	my $self = shift;
	my $accum = shift;

	bless {
		%$self, 
		left => $self->left->splat($accum),
		right => $self->right->splat($accum),
	}, (ref $self);
}

sub free_vars {
	my $self = shift;
	map { $_->free_vars} $self->left, $self->right;
}

package type::operator::nary;
use base qw/type::operator/;

sub new {
	my $class = shift;
	my $self = $class->SUPER::new;
	$self->{name} = shift;
	$self->{param} = shift;

	$self;
}

sub splat {
	my $self = shift;
	my $accum = shift;

	bless {
		%$self,
		param => $self->{param}->splat($accum),
	}, (ref $self);
}

sub free_vars {
	my $self = shift;
	$self->{param}->free_vars;
}


package type::operator::arrow;
use base qw/type::operator::binary/;

sub symbol { "→" }
sub is_arrow { 1 }

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

sub free_vars { $_[0] }

package type::variable::instantiated;
use base qw/type::variable/;

sub is_arrow {
	my $self = shift;
	$self->{instance}->is_arrow;
}

sub splat { # this simplifies the AG above, and avoids unnecessary duplication
	my $self = shift;
	$self->{instance}->splat(@_);
}

sub left {
	my $self = shift;
	$self->{instance}->left;
}

sub right {
	my $self = shift;
	$self->{instance}->right;
}

sub free_vars {
	my $self = shift;
	$self->{instance}->free_vars;
}

package node;
# all AST nodes inherit from this

sub new {
	my $class = shift;
	bless {
		t => undef,
		(@_ == 1 ? (v => $_[0]) : @_), # an easy constructor
	}, $class;
}

# functions are applied 
package fun;
use base qw/node/;

package comb;
use base qw/node/;

package ide;
use base qw/node/;

package let;
use base qw/node/;

package cond;
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
multi unify => ( 'type::variable::free', any(qw/type::variable::free type::operator/) ) => sub {
	return if ($_[0] == $_[1]); # in a recursive let expr a generic recursive function will be reunified with it's still free variable
	$_[0]->instantiate($_[1]);
};
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
	
	unify($x->left, $y->left, @_);
	unify($x->right, $y->right, @_);
};

multi unify => ('type::operator::nary', 'type::operator::nary') => sub {
	my ($x, $y) = (shift, shift);

	$x->{name} eq $y->{name}
		or die "Can't unify $x->{name} with $y->{name} in " . Dumper(@_);

	unify($x->{param}, $y->{param}, @_);
};

multi unify => ('type', 'type') => sub {
	die "trying to unify incompatible types:\n" . Dumper(@_);
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
	sub => type::operator::arrow->new(
		type::operator::nullary->new("int"),	
		type::operator::arrow->new(
			type::operator::nullary->new("int"),
			type::operator::nullary->new("int"),
		),
	),
	mul => type::operator::arrow->new(
		type::operator::nullary->new("int"),	
		type::operator::arrow->new(
			type::operator::nullary->new("int"),
			type::operator::nullary->new("int"),
		),
	),
	# a -> list a -> list a
	cons => do {
		my $a = type::variable::free->new;
		type::operator::arrow->new(
			$a,
			type::operator::arrow->new(
				type::operator::nary->new(list => $a),
				type::operator::nary->new(list => $a),
			),
		);
	},
	# list a
	nil => type::operator::nary->new(list => type::variable::free->new),
	# list a -> bool
	'is_nil?' => type::operator::arrow->new(
		type::operator::nary->new(list => type::variable::free->new),
		type::operator::nullary->new("bool"),
	),
	# list a -> a
	head => do {
		my $a = type::variable::free->new;
		type::operator::arrow->new(
			type::operator::nary->new(list => $a),
			$a,
		);
	},
	# list a -> list a
	tail => do {
		my $a = type::variable::free->new;
		my $l = type::operator::nary->new(list => $a);
		type::operator::arrow->new( $l, $l );
	},
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
	# int -> int -> bool
	'==' => type::operator::arrow->new(
		type::operator::nullary->new("int"),
		type::operator::arrow->new(
			type::operator::nullary->new("int"),
			type::operator::nullary->new("bool"),
		),
	),
);


package main;

use strict;
use warnings;

use Test::More 'no_plan';
use Test::Exception;
use Data::Dumper;
use Language::AttributeGrammar;

{
	# please forgive me
	my $orig = \&Test::Builder::output;
	no warnings 'redefine';
	*Test::Builder::output = sub {
		my $ret = &$orig(@_);
		binmode $ret, ":utf8";
		$ret;
	};
}

$Data::Dumper::Terse = 1; # no need for pointless $VARx
$Data::Dumper::Indent = 1;


sub AUTOLOAD { # an easy constructor... fun() instead of fun->new()
	(my $class) = ((our $AUTOLOAD) =~ /([^:]+) $/x);
	$class->new(@_);
}


# this is the actual inferrence engine
my $i = Language::AttributeGrammar->new(<<'#\'EOG');
#\

# at the root we initialize the predefined symbols, and the initial set of generic variables
ROOT: env($$) = { Clone::clone(\%types::prelude) }
ROOT: generic_in($$) = {
	my $env = env($$);

	Set::Object->new(
		grep { $_->free_vars }
			map { $_->left }
				grep { $_->is_arrow }
					values %$env
	);
}

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
	my $t_of_param = type($$)->left;
	my $prev_generic = generic($.body);

	if ($t_of_param->free_vars) {
		return $prev_generic + Set::Object->new($t_of_param);
	} else {
		return $prev_generic;
	}
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

	types::unify(
		$f,
		type::operator::arrow->new(
			type::variable::free->new,
			type::variable::free->new,
		),
		$.fun,
		$.param,
	);

	# first of all the type of the thing we're appling must be a function
	die "Can't apply non function type (" . $.fun . " has type " . $f->stringify
		unless $f->is_arrow;
	
	
	my $p_f = $f->left;
	my $ret = $f->right;
	
	my $generic_vars = generic($.fun);

	# if the function is generic we'll be instantiating it's type variables in the function body
	# for this reason the function needs to be cloned (so it will stay generic when we leave the body of the function)
	if ($generic_vars->includes($p_f)) {
		my $c = $f->clone;
		($p_f, $ret) = ($c->left, $c->right);
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
let: new_env($$) = {
	my $orig = env($$);
	my %new = (
		%$orig,
		($.ide) => type::variable::free->new,
	);
	\%new;
}
let: env($.is) = { new_env($$) }
let: env($.in) = { new_env($$) }

let: type($$) = {
	types::unify(
		new_env($$)->{ $.ide }, type($.is),
		$.ide, $.is,
	);


	type($.in);
}

cond: env($.if) = { env($$) }
cond: env($.then) = { env($$) }
cond: env($.else) = { env($$) }

cond: generic_in($.if) = { generic_in($$) }
cond: generic_in($.then) = { generic($.if) }
cond: generic_in($.else) = { generic($.then) }
cond: generic($$) = { generic($.else) }

cond: type($$) = {
	types::unify(
		type($.if), type::operator::nullary->new("bool"),
		$.if, "bool",
	);

	types::unify(
		my $t = type($.then), type($.else),
		$.then, $.else
	);

	$t;
}


#'EOG


# these are some sample ASTs that need to be typed

# this is a function that expects an int and will emit an int
# add (int -> int -> int) is resolved using val(10) into int -> int
# x is inferred to be int
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

# fun(x) x	-  takes any value and returns it
# x stays free, and thus becomes generic (id is polymorphic)
my $id = fun(
	param => "x",
	body => ide("x"),
);

# plus_ten(15) (int -> int applied to int) is a number
my $fifteen = comb(
	fun => $plus_ten,
	param => val(5),
);

# the pair function takes any param and returns a function that takes any param
# that function will return a pair with the first param and the second param inside a pair
# hence pair is a -> b -> (a x b). So if a is an int, $half_pair is b -> (int x b)
my $half_pair = comb(
	fun => ide("pair"),
	param => val(10),
);

# a fully constructed pair, with a = int and b = str
my $pair = comb(
	fun => $half_pair,
	param => val(v => "foo", t => type::operator::nullary->new("str")),
);

# the polymorphic snd function takes any pair and returns it's second thingy
# (a x b) -> b
# applied to $pair (int x str) this should e str
my $snd = comb(
	fun => ide("snd"),
	param => $pair,
);

# the polymorphic fst function takes any pair and returns it's first thingy
# (a x b) -> a
# applied to $pair (int x str) this should be int
my $fst = comb(
	fun => ide("fst"),
	param => $pair,
);

# this makes sure that ':t id(10)' is the same as ':t 10'
my $app_id = let(
	ide => "id",
	is => $id,
	in => comb(
		fun => ide("id"),
		param => val(10),
	),
);

# this expr is like the following haskell:
# let id x = x
# in id ((id 3), (id "foo"))
# and basically ensures that id is polymorphic when used in a let expr.
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

# this function cannot be typed, since it wants a function in x, (a -> b), and a is both an int and a str
# while this could seem to make sense for e.g. the 'id' function, this won't work if we pass in $plus_ten
my $flatten_polymorphic = fun(
	param => "f",
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

# if we don't know what symbol it is we can't type it
my $no_such_sym = fun(
	param => "x",
	body => ide("y"),
);

# this is a higher order function that takes a function and applies it to a string
# thiss tests that the inferrence of higher order functions is correct
my $app_to_str = fun(
	param => "x",
	body => comb(
		fun => ide("x"),
		param => val(v => "foo", t => type::operator::nullary->new("str")),
	),
);

# we can apply a polymorphic function to a string by giving it as a param to a higher order function
my $app_id_to_str = comb(
	fun => $app_to_str,
	param => $id,
);

# we can't get app_to_str to apply something of a bad type to a str
my $app_plus_ten_to_str = comb(
	fun => $app_to_str,
	param => $plus_ten,
);

my $fac = fun(
	param => "x",
	body => let(
		ide => "fac'",
		is => fun(
			param => "x",
			body => fun(
				param => "r",
				body => cond(
					if => comb(
						fun => comb(
							fun => ide("=="),
							param => ide("x"),
						),
						param => val(0),
					),
					then => ide("r"),
					else => comb(
						fun => comb(
							fun => ide("fac'"),
							param => comb(
								fun => comb(
									fun => ide("sub"),
									param => ide("x"),
								),
								param => val(1),
							),
						),
						param => comb(
							fun => comb(
								fun => ide("mul"),
								param => ide("r"),
							),
							param => ide("x"),
						),
					),
				),
			),
		),
		in => comb(
			fun => comb(
				fun => ide("fac'"),
				param => ide("x"),
			),
			param => val(1),
		),
	),
);

my $length = fun(
    param => "xs",
    body => let(
        ide => "length'",
        is => fun(
            param => "xs",
            body => fun(
                param => "n",
                body => cond(
                    if => comb(
                        fun => ide("is_nil?"),
                        param => ide("xs"),
                    ),
                    then => val("n"),
                    else => comb(
                        fun => comb(
                            fun => ide("length'"),
                            param => comb(
								fun => ide("tail"),
								param => ide("xs"),
							),
						),
						param => comb(
							fun => comb(
								fun => ide("add"),
								param => ide("n"),
							),
							param => val(1),
						),
					),
				),
            ),
        ),
        in => comb(
            fun => comb(
                fun => ide("length'"),
                param => ide("xs"),
            ),
            param => val(0),
        ),
    ),
);

my $fac_5 = comb(
	fun => $fac,
	param => val(5),
);

my $length_and_first = fun(
	param => "xs",
	body => comb(
		fun => comb(
			fun => ide("pair"),
			param => comb(
				fun => ide("head"),
				param => ide("xs"),
			),
		),
		param => comb(
			fun => $length,
			param => ide("xs"),
		),
	),
);

my $one_elem_list = fun(
	param => "x",
	body => comb(
		fun => comb(
			fun => ide("cons"),
			param => ide("x"),
		),
		param => ide("nil"),
	),
);

my $length_and_first_p = let(
	ide => "listify",
	is => $one_elem_list,
	in => let(
		ide => "length_and_first",
		is => $length_and_first,
		in => comb(
			fun => ide("length_and_first"),
			param => comb(
				fun => ide("listify"),
				param => val(10),
			),
		),
	),
);

my $length_and_first_gen = let(
	ide => "listify",
	is => $one_elem_list,
	in => let(
		ide => "length_and_first",
		is => $length_and_first,
		in => comb(
			fun => comb(
				fun => ide("pair"),
				param => comb(
					fun => ide("length_and_first"),
					param => comb(
						fun => ide("listify"),
						param => val(10),
					),
				),
			),

			param => comb(
				fun => ide("length_and_first"),
				param => comb(
					fun => ide("listify"),
					param => val(v => "foo", t => type::operator::nullary->new("str")),
				),
			),
		),
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
{
	my $type = type::operator::arrow->new(
		type::operator::arrow->new(
			type::variable::free->new,
			type::variable::free->new,
		),
		type::variable::free->new,
	);
	is($type->stringify, "∀α. ∀β. ∀γ. (α → β) → γ", "pretty print (a -> b) -> c");
}
{
	my $type = type::operator::arrow->new(
		type::variable::free->new,
		type::operator::arrow->new(
			type::variable::free->new,
			type::variable::free->new,
		),
	);
	is($type->stringify, "∀α. ∀β. ∀γ. α → β → γ", "pretty print a -> b -> c");
}
{
	my $type = type::operator::pair->new(
		type::variable::free->new,
		type::variable::free->new,
	);
	is($type->stringify, "∀α. ∀β. (α × β)", "pretty print a x b");
}
{
	my $type = type::operator::arrow->new(
		type::variable::free->new,
		type::operator::pair->new(
			type::variable::free->new,
			type::variable::free->new,
		),
	);
	is($type->stringify, "∀α. ∀β. ∀γ. α → (β × γ)", "pretty print a -> b x c");
}
{
	my $type = type::operator::arrow->new(
		type::operator::pair->new(
			type::variable::free->new,
			type::variable::free->new,
		),
		type::variable::free->new,
	);
	is($type->stringify, "∀α. ∀β. ∀γ. (α × β) → γ", "pretty print a x b -> c");
}
{
	my $type = type::operator::nary->new(list => type::variable::free->new);
	is($type->stringify, "∀α. list α", "pretty print list a");
}
{
	my $a = type::variable::free->new;
	my $type = type::operator::arrow->new(
		type::operator::nary->new(list => $a),
		$a,
	);
	is($type->stringify, "∀α. list α → α", "pretty print list a -> a");
}
{
	my $a = type::variable::free->new;
	my $b = type::variable::free->new;
	my $type = type::operator::arrow->new(
		type::operator::nary->new(list => $a),
		type::operator::arrow->new(
            type::operator::nary->new(list => $b),
            type::operator::nary->new(list => type::operator::pair->new( $a, $b )),
		),
	);
	is($type->stringify, "∀α. ∀β. list α → list β → list (α × β)", "pretty print :t zip");
}




# tests for the type inferrencer
is(t($id), "∀α. α → α", ":t id");
is(t($app_id), "int", ":t id(3)");
is(t($plus_ten), "int → int", ":t (+ 10)");
is(t($fifteen), "int", ":t (+ 10) 5");
is(t($half_pair), "∀α. α → (int × α)", ":t pair(10)");
is(t($pair), "(int × str)", ":t pair(10)('foo')");
is(t($fst), "int", ":t fst( pair(10)('foo') )");
is(t($snd), "str", ":t snd( pair(10)('foo') )");
is(t(ide("pair")), "∀α. ∀β. α → β → (α × β)", ":t pair");
is(t(ide("fst")), "∀α. ∀β. (α × β) → α", ":t fst");
is(t(ide("snd")), "∀α. ∀β. (α × β) → β", ":t snd");
is(t($id_pair), "(int × str)", ":t id(pair(id(10))(id('foo')))");
is(t($app_to_str), "∀α. (str → α) → α", ":t (λx. (x 'str'))");
is(t($app_id_to_str), "str", ":t (λx. (x 'str'))(id)");
is(t($fac), "int → int", ":t fac");
is(t($length), "∀α. list α → int", ":t length");
is(t($fac_5), "int", ":t fac 5");
is(t($one_elem_list), "∀α. α → list α", ":t listify(x)");
is(t(comb( fun => $one_elem_list, param => val(5) )), "list int", ":t listify(5)");
is(t($length_and_first), "∀α. list α → (α × int)", ":t length_and_first");
is(t($length_and_first_p), "(int × int)", ":t length_and_first [1]");
is(t($length_and_first_gen), "((int × int) × (str × int))", ":t (length_and_first ['foo'], length_and_first [1])");

dies_ok { t($flatten_polymorphic) } "can't type λf.(pair (f 3))(f 'foo')";
dies_ok { t($no_such_sym) } "all identifiers need to be resolvable";
dies_ok { t($app_plus_ten_to_str) } "can't do a higher order app with uneq types";

# a helper that returns the stringified type of an AST
sub t {
	my $ast = shift;
	$i->apply($ast)->type->stringify;
}


