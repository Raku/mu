LREP:=perl lrep

all: prelude tests lrep

suite: clean all

prelude: Grammar/Perl6.pm Grammar/Rules.pm Grammar/Perl6Primitives.pm

Grammar/Perl6.pm: Grammar/Perl6.p6
	cp Grammar/Perl6.pm Grammar/Perl6.pm.cache
	$(LREP) -i Grammar/Perl6.p6 -o Grammar/Perl6.pm

Grammar/Rules.pm: Grammar/Rules.p6
	cp Grammar/Rules.pm Grammar/Rules.pm.cache
	$(LREP) -i Grammar/Rules.p6 -o Grammar/Rules.pm

Grammar/Perl6Primitives.pm: Grammar/Perl6Primitives.p6
	$(LREP) -i Grammar/Perl6Primitives.p6 -o Grammar/Perl6Primitives.pm

tests: t/p6sample.t t/p6compiler.t t/iterator_engine.t t/macro.t
	perl t/macro.t
	perl t/p6sample.t
	perl t/p6compiler.t
	perl t/iterator_engine.t

t/p6sample.t: t/p6sample.p6t Grammar/Perl6.pm
	$(LREP) -i t/p6sample.p6t -o t/p6sample.t

t/p6compiler.t: t/p6compiler.p6t Grammar/Perl6.pm Grammar/Perl6Primitives.pm
	$(LREP) -i t/p6compiler.p6t -o t/p6compiler.t

t/macro.t: t/macro.p6t
	$(LREP) -i t/macro.p6t -o t/macro.t

lrep: prelude lrep.p6 Grammar/Perl6.pm
	cp lrep lrep.cache
	$(LREP) -i lrep.p6 -o lrep

bootstrap: suite
	cp Grammar/Perl6.pm.cache Grammar/Perl6.pm.bak
	cp lrep.cache lrep.bak
	make suite
	@echo ">>>>> Begin Differences caused by bootstrap"
	@diff lrep lrep.cache
	@diff Grammar/Perl6.pm Grammar/Perl6.pm.cache
	@echo "<<<<< End Differences caused by bootstrap"


clean:
	rm -f Grammar/Perl6Primitives.pm
	touch Grammar/Perl6.p6
	rm -f t/macro.t
	rm -f t/p6sample.t
	rm -f t/p6compiler.t
