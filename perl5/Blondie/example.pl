#!/usr/bin/perl

use strict;
use warnings;

use Blondie::Nodes;
use Blondie::Backend::Perl::Silly;
use Blondie::Emitter::Pretty;
use Blondie::TypeSafe;

my $r = Blondie::Backend::Perl::Silly->new;

my $x = 15;

my $prog = App(
	Sym('&print'),
	Sym('$*OUT'),
	App(
		Sym('&infix:<~>'),
		Val(42),
		Val("\n"),
	),
);

$prog = App(
	Sym('&say'),
	Val(42),
);

my $t1 = times;
my $c = $r->compile($prog);
my $t2 = times;
my $safe = Blondie::TypeSafe::Annotator->new->typecheck($r, $c);
my $t3 = times;
my $res  = $r->execute($safe);
my $t4 = times;


my $pretty = Blondie::Emitter::Pretty->new;

print "Resulting AST: " . $pretty->string($res), "\n";
printf "Total time: %.3f seconds (%.3f compilation, %.3f type checking, %.3f execution)\n", $t4 - $t1, $t2 - $t1, $t3 - $t2, $t4 - $t3;

print "Compiled: " . $pretty->string($c) . "\n";
print "Type safe: " . $pretty->string($safe) . "\n";


