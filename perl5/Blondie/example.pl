#!/usr/bin/perl

use strict;
use warnings;

use Blondie::Nodes;
use Blondie::Backend::Perl::Silly;
use Blondie::Emitter::Pretty;

my $r = Blondie::Backend::Perl::Silly->new;

my $x = 15;

my $prog = Seq(
    map {
        App(
            Sym('&print'),
            Sym('$*OUT'),
            Val("(($_ ** $x) / ($_ ** ($x - 1)) = "),
        ),
        App(
            Sym('&say'),
            App(
                Sym('&infix:</>'),
                App(
                    Sym('&infix:<**>'),
                    Val($_),
                    Val($x),
                ),
                App(
                    Sym('&infix:<**>'),
                    Val($_),
                    Val($x-1),
                ),
            ),
        ),
    } 1 .. 10,
);

my $t1 = times;
my $c = $r->compile($prog);
my $t2 = times;
my $res  = $r->execute($c);
my $t3 = times;

my $pretty = Blondie::Emitter::Pretty->new;

print "Resulting AST: " . $pretty->string($res), "\n";
printf "Total time: %.3f seconds (%.3f compilation, %.3f execution)\n", $t3 - $t1, $t2 - $t1, $t3 - $t2;

# print "Compiled: " . $pretty->string($c);


