all: prelude tests

prelude: Grammar/Perl6.pm Grammar/Perl6Primitives.pm

Grammar/Perl6.pm: Grammar/Perl6.p6
	./lrep.pl Grammar/Perl6.p6 Grammar/Perl6.pm

Grammar/Perl6Primitives.pm: Grammar/Perl6Primitives.p6
	./lrep.pl Grammar/Perl6Primitives.p6 Grammar/Perl6Primitives.pm

tests: t/p6sample.t t/p6compiler.t t/iterator_engine.t
	perl t/p6sample.t
	perl t/p6compiler.t
	perl t/iterator_engine.t

t/p6sample.t: t/p6sample.p6t
	./lrep.pl t/p6sample.p6t t/p6sample.t

t/p6compiler.t: t/p6compiler.p6t
	./lrep.pl t/p6compiler.p6t t/p6compiler.t