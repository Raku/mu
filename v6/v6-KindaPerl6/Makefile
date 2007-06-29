all: lib5/KindaPerl6/Grammar.pm lib5/KindaPerl6/Grammar/Regex.pm lib5/KindaPerl6/Traverse.pm lib5/KindaPerl6/Visitor/Hyper.pm lib5/KindaPerl6/Visitor/EmitPerl5.pm lib5/KindaPerl6/Visitor/Perl.pm lib5/KindaPerl6/Visitor/LexicalSub.pm lib5/KindaPerl6/Visitor/MetaClass.pm lib5/KindaPerl6/Visitor/Token.pm lib5/KindaPerl6/Runtime/Perl5/*.pm
lib5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	cp $< $@
lib5/%.pm: lib/%.pm
	perl mp6.pl < $< > $@
