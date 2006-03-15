all: prelude tests lrep

suite: clean all

prelude: Grammar/Perl6.pm Grammar/Perl6Primitives.pm

Grammar/Perl6.pm: Grammar/Perl6.p6
	./lrep.pl -i Grammar/Perl6.p6 -o Grammar/Perl6.pm

Grammar/Perl6Primitives.pm: Grammar/Perl6Primitives.p6
	./lrep.pl -i Grammar/Perl6Primitives.p6 -o Grammar/Perl6Primitives.pm

tests: t/p6sample.t t/p6compiler.t t/iterator_engine.t# t/macro.t
#	perl t/macro.t
	perl t/p6sample.t
	perl t/p6compiler.t
	perl t/iterator_engine.t

t/p6sample.t: t/p6sample.p6t Grammar/Perl6.pm
	./lrep.pl -i t/p6sample.p6t -o t/p6sample.t

t/p6compiler.t: t/p6compiler.p6t Grammar/Perl6.pm Grammar/Perl6Primitives.pm
	./lrep.pl -i t/p6compiler.p6t -o t/p6compiler.t

#t/macro.t: t/macro.p6t
#	./lrep.pl -i t/macro.p6t -o t/macro.t

lrep: prelude lrep.p6 Grammar/Perl6.pm Grammar/Perl6Primitives.pm
	./lrep.pl -i lrep.p6 -o lrep

clean:
	rm -f Grammar/Perl6Primitives.pm
	touch Grammar/Perl6.p6
	rm -f t/macro.t
	rm -f t/p6sample.t
	rm -f t/p6compiler.t
