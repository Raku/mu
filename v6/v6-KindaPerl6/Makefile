kp6_mp6_OBJ = $(patsubst lib-kp6-mp6/%,lib-kp6-mp6-p5/%,$(wildcard lib-kp6-mp6/KindaPerl6/Grammar/*.pm lib-kp6-mp6/KindaPerl6/Grammar.pm lib-kp6-mp6/KindaPerl6/Traverse.pm lib-kp6-mp6/KindaPerl6/Ast.pm lib-kp6-mp6/KindaPerl6/Visitor/*.pm lib-kp6-mp6/KindaPerl6/Runtime/Perl5/*.pm lib-kp6-mp6/KindaPerl6/Runtime/Perl6/*.pm lib-kp6-mp6/KindaPerl6/Test/*.pm ))

kp6_kp6_OBJ = $(patsubst lib-kp6-kp6/%,lib-kp6-kp6-p5/%,$(wildcard lib-kp6-kp6/KindaPerl6/Grammar/*.pm lib-kp6-kp6/KindaPerl6/Grammar.pm lib-kp6-kp6/KindaPerl6/Traverse.pm lib-kp6-kp6/KindaPerl6/Ast.pm lib-kp6-kp6/KindaPerl6/Visitor/*.pm lib-kp6-kp6/KindaPerl6/Runtime/Perl5/*.pm lib-kp6-kp6/KindaPerl6/Runtime/Perl6/*.pm lib-kp6-kp6/KindaPerl6/Test/*.pm ))

modules_OBJ = $(patsubst lib-modules/%,lib-modules-p5/%,$(wildcard lib-modules/*.pm))

all: $(kp6_mp6_OBJ) $(kp6_kp6_OBJ) $(modules_OBJ)

lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib-kp6-mp6/KindaPerl6/Runtime/Perl6/%.pm
	@echo COMPILE $< $@
	@perl kp6-mp6-perl5.pl < $< | perltidy > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi
lib-modules-p5/%.pm: lib-modules/%.pm
	@echo COMPILE $< $@
	@perl kp6-mp6-perl5.pl < $< | perltidy > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib-kp6-kp6/KindaPerl6/Runtime/Perl6/%.pm
	perl kp6-mp6-perl5.pl < $< | perltidy > $@


lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib-kp6-mp6/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@

lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib-kp6-kp6/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@

lib-kp6-mp6-p5/%.pm: lib-kp6-mp6/%.pm
	perl mp6.pl < $< > $@

lib-kp6-kp6-p5/%.pm: lib-kp6-kp6/%.pm
	perl kp6-mp6-perl5.pl < $< | perltidy > $@

kp6-kp6.pl: kp6.p6
	perl kp6-kp6-perl5.pl < $< | perltidy > temp.pl ; \
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
	find lib-kp6-mp6/KindaPerl6/ -name "*.pm"| grep -v \.svn |xargs touch
	find lib-kp6-kp6/KindaPerl6/ -name "*.pm"| grep -v \.svn |xargs touch

clean: 
	echo "make clean would destroy the compiler"
	#rm -f $(OBJ)

# for debugging:
echo:
	echo $(OBJ)
.PHONY: clean test
