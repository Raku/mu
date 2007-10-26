onion_OBJ = $(patsubst src/%,compiled/perl5/lib/%,$(wildcard src/*.pm src/Onion/Grammar/*.pm src/Onion/Grammar.pm src/Onion/Traverse.pm src/Onion/Ast.pm src/Onion/Visitor/*.pm src/Onion/Visitor/Emit/*.pm src/Onion/Runtime/Perl6/*.pm ))
compiled/perl5/lib/%.pm: src/%.pm
	
	perl -I../v6-KindaPerl6/compiled/perl5-kp6-mp6/lib/ ../v6-KindaPerl6/script/kp6 -Cperl5rx < $< > $@
all: $(onion_OBJ)
