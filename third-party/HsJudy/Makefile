#GHC=ghc-6.4.2 
GHC=ghc # -debug

CC=$(GHC)
TESTS=TestBS TestJL TestJSL TestJHS TestHash TestIntMap TestStrMap TestDM TestHT
#HSC2HS=/usr/bin/hsc2hs
HSC2HS=hsc2hs


CFLAGS= -O2
LIB_JUDY=-lJudy

CHEADERS= Judy/Private_hsc.h
HSFLAGS= $(HSF) -O2 -fffi -I/usr/local/include -fglasgow-exts -I. '-\#include Judy/Private_hsc.h' $(LIB_JUDY)


%.hi: %.o
	@:
%_hsc.c: %.hs
	@:
%_hsc.h: %.hs
	@:

%.hs: %.hsc
	$(HSC2HS) -C "-I/usr/local/include" -C "-I$(JUDY_SRC)/src" -C "$(CFLAGS)" -o $@ $<

all: $(TESTS)

Judy_hsc.c: Judy_hsc.h

%: tests/%.hs Judy/*.hs Judy/Private.hs Judy/Private_hsc.o tests/Test.hs
	$(GHC) $(HSFLAGS) --make $< tests/Test.hs Judy/Private_hsc.o -o tests/$@ 

.PHONY: test clean

test: $(TESTS)
	cd tests; echo; env HARNESS_PERL="perl -e \"exec './' . pop; \" " prove $(TESTS)

clean:
	cd tests; rm -f $(TESTS); cd ..
	rm -f $(TESTS) Judy/Private.hs Judy/*_stub.? Judy/*_hsc.c Judy/*_hsc.h Judy/*.hi Judy/*.o *.hi *.o tests/*.hi tests/*.o
