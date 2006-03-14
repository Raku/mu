all: prelude tests

prelude: Grammar/Perl6.pm Grammar/Perl6Primitives.pm

Grammar/Perl6.pm: Grammar/Perl6.p6
	./lrep.pl -i Grammar/Perl6.p6 -o Grammar/Perl6.pm

Grammar/Perl6Primitives.pm: Grammar/Perl6Primitives.p6
	./lrep.pl -i Grammar/Perl6Primitives.p6 -o Grammar/Perl6Primitives.pm

tests: t/p6sample.t t/p6compiler.t t/iterator_engine.t
	perl t/p6sample.t
	perl t/p6compiler.t
	perl t/iterator_engine.t

t/p6sample.t: t/p6sample.p6t
	./lrep.pl -i t/p6sample.p6t -o t/p6sample.t

t/p6compiler.t: t/p6compiler.p6t
	./lrep.pl -i t/p6compiler.p6t -o t/p6compiler.t

lrep: lrep.p6
	./lrep.pl -i lrep.p6 -o lrep
