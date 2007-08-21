OBJ = $(patsubst lib/%,lib5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/Prelude.pm lib/KindaPerl6/Runtime/Perl6/Pair.pm lib/KindaPerl6/Runtime/Perl6/Hash.pm))
all: $(OBJ)

lib5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	perl kp6-perl5.pl < $< | perltidy > temp.pm ; cp temp.pm $@
lib5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@
lib5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@
test:
	perl run_tests.pl

clean: 
	rm -f $(OBJ)

# for debugging:
echo:
	echo $(OBJ)
.PHONY: clean test
