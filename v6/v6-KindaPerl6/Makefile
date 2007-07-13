all: lib5/KindaPerl6/Grammar.pm lib5/KindaPerl6/Grammar/Regex.pm lib5/KindaPerl6/Traverse.pm lib5/KindaPerl6/Visitor/*.pm lib5/KindaPerl6/Runtime/Perl5/*.pm
lib5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@
lib5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@
clean: 
	rm -f lib5/KindaPerl6/Grammar.pm lib5/KindaPerl6/Grammar/Regex.pm lib5/KindaPerl6/Traverse.pm lib5/KindaPerl6/Visitor/*.pm lib5/KindaPerl6/Runtime/Perl5/*.pm
.PHONY: clean
