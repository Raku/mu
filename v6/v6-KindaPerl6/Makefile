																				OBJ=$(shell echo lib5/KindaPerl6/Grammar.pm lib5/KindaPerl6/Grammar/{Control,Mapping,Regex}.pm lib5/KindaPerl6/Traverse.pm lib5/KindaPerl6/Visitor/{CreateEnv,EmitAstHTML,EmitHTML,EmitPerl5,EmitPerl5Regex,EmitPerl6,Global,Hyper,LexicalSub,MetaClass,Perl,RegexCapture,Token}.pm lib5/KindaPerl6/Runtime/Perl5/{Code,DispatchSugar,Match,Runtime,Wrap,Compiler,MOP,Pad,Type}.pm lib5/KindaPerl6/Runtime/Perl6/Prelude.pm)

all: $(OBJ)

lib5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	perl kp6-perl5.pl < $< | perltidy > $@
lib5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@
lib5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@

test:
	sh run_tests.sh

clean: 
	rm -f $(OBJ)

# for debugging:
echo:
	echo $(OBJ)
.PHONY: clean test
