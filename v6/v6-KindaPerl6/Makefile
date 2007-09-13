kp6_mp6_OBJ = $(patsubst lib/%,lib-kp6-mp6-p5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Ast.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/*.pm lib/KindaPerl6/Test/*.pm ))

kp6_kp6_OBJ = $(patsubst lib/%,lib-kp6-kp6-p5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Ast.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/*.pm lib/KindaPerl6/Test/*.pm ))

kp6_mp6_modules_OBJ = $(patsubst lib-modules/%,lib-modules-kp6-mp6-p5/%,$(wildcard lib-modules/*.pm))

kp6_kp6_modules_OBJ = $(patsubst lib-modules/%,lib-modules-kp6-kp6-p5/%,$(wildcard lib-modules/*.pm))

all: $(kp6_mp6_OBJ) $(kp6_kp6_OBJ) $(kp6_mp6_modules_OBJ) $(kp6_kp6_modules_OBJ) kp6-kp6.pl

lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	@echo COMPILE $< $@
	@perl kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi
lib-modules-kp6-mp6-p5/%.pm: lib-modules/%.pm
	@echo COMPILE $< $@
	@perl kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

lib-modules-kp6-kp6-p5/%.pm: lib-modules/%.pm
	@echo COMPILE $< $@
	@KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' perl kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi


lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' perl kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@


lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@

lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@

lib-kp6-mp6-p5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@

lib-kp6-kp6-p5/%.pm: lib/%.pm
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' perl kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@

kp6-kp6.pl: kp6.p6
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' perl kp6-kp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > temp.pl ; \
		res=$$? ; \
		if [ $$res = 0 -a -s temp.pl ] ; then \
			cp temp.pl $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi; \
	    rm temp.pl



test: all
	perl run_tests_kp6_kp6.pl

test_grammar: all
	perl run_tests_kp6_kp6.pl --section=grammar

test_io: all
	perl run_tests_kp6_kp6.pl --section=io

test_math: all
	perl run_tests_kp6_kp6.pl --section=math

test_base: all
	perl run_tests_kp6_kp6.pl --section=''

forcerecompile:
	echo "use -B instead"
	find lib/KindaPerl6/ -name "*.pm"| grep -v \.svn |xargs touch

clean: 
	echo "make clean would destroy the compiler"
	#rm -f $(OBJ)

# for debugging:
echo:
	echo $(OBJ)
.PHONY: clean test
