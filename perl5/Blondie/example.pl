#!/usr/bin/perl

use strict;
use warnings;

use Blondie::Nodes;
use Blondie::Backend::C;
use Blondie::Emitter::Pretty;
use Blondie::TypeSafe;

my $r = Blondie::Backend::C->new;

my $x = 15;

my $prog = Seq(
	App(
		Sym('&say'),
		Val(42),
	),
);

my $t1 = times;
my $c = $r->compile($prog);
my $t2 = times;
my $annotated = $r->annotate($c);
my $t3 = times;
my $res  = $r->emit($annotated);
my $t4 = times;
my $f = $r->bind($res);
my $t5 = times;
&$f;
my $t6 = times;

printf "Total time: %.3f seconds (%.3f compilation, %.3f type checking, %.3f emission, %.3f binding, %.3f execution)\n", $t6 - $t1, $t2 - $t1, $t3 - $t2, $t4 - $t3, $t5 - $t4, $t6 - $t5;

print "C code:\n$res\n";

#print "Compiled: " . $pretty->string($c) . "\n";
#print "Type safe: " . $pretty->string($safe) . "\n";
