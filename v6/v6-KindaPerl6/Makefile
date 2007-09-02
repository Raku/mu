OBJ = $(patsubst lib/%,lib5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Ast.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/*.pm lib/KindaPerl6/Test/*.pm ))

all: $(OBJ)

lib5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	@echo COMPILE $< $@
	@perl kp6-perl5.pl < $< | perltidy > temp.pm ; \
		res=$$? ; \
		if [ $$res = 0 -a -s temp.pm ] ; then \
			cp temp.pm $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi
lib5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@
lib5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@
test: all
	perl run_tests.pl

test_grammar: all
	perl run_tests.pl --section=grammar

test_io: all
	perl run_tests.pl --section=io

test_math: all
	perl run_tests.pl --section=math

test_base: all
	perl run_tests.pl --section=''

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
