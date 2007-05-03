#all:
#	mkdir -p blib/lib/Ook
#	cp lib/Ook/Grammar.pm blib/lib/Ook/
#
#clean:
#	rm -rf blib

test:
	for test in t/*.t t/*/*.t; do\
		echo $$test &&\
		../../../../pugs -I ../../../../ext/Test/lib -I lib $$test;\
	done
