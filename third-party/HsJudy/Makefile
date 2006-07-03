#GHC=ghc-6.4.2 
GHC=ghc # -debug
CC=$(GHC)
TESTS=TestBS TestJL TestJSL TestJHS TestMap TestMap2

CFLAGS= -O2

JUDY_SRC=../judy/Judy-1.0.3


LIB_JUDY=-lJudy

# static linking
#LIB_JUDY=$(JUDY_SRC)/src/Judy1/*.o $(JUDY_SRC)/src/JudyL/*.o $(JUDY_SRC)/src/JudySL/*.o $(JUDY_SRC)/src/JudyHS/*.o $(JUDY_SRC)/src/JudyCommon/*.o
 

CHEADERS= Judy/Private_hsc.h
HSFLAGS= $(HSF) -O2 -fffi -I/usr/local/include -fglasgow-exts -I. '-\#include Judy/Private_hsc.h' $(LIB_JUDY)

%.hi: %.o
	@:
%_hsc.c: %.hs
	@:
%_hsc.h: %.hs
	@:

%.hs: %.hsc
	hsc2hs -C "-I/usr/local/include" -C "$(CFLAGS)" -o $@ $<


all: try $(TESTS)

Judy_hsc.c: Judy_hsc.h

# fixme: DRY

%: tests/%.hs Judy/*.hs Judy/Private.hs Judy/Private_hsc.o
	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o tests/$@
##
##
##testbs: tests/TestBS.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
##testjl: tests/TestJL.hs Judy/Private.hs Judy/Private_hsc.o 
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
##testjsl: tests/TestJSL.hs Judy/Private.hs Judy/Private_hsc.o 
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
##testjhs: tests/TestJHS.hs Judy/Private.hs Judy/Private_hsc.o 
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
##testmap: tests/TestMap.hs Judy/Map.hs Judy/Private.hs Judy/Private_hsc.o
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
##testmap2: tests/TestMap2.hs Judy/Map2.hs Judy/Private.hs Judy/Private_hsc.o
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
##
try: Try.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

##trybench: TryBench.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
##	$(GHC) $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

.PHONY: test clean

test: $(TESTS)
	@echo
	@for i in $(TESTS); do ./tests/$$i ; echo ; done

clean:
	cd tests; rm -f $(TESTS); cd ..
	rm -f try $(TESTS) Judy/Private.hs Judy/*_stub.? Judy/*_hsc.c Judy/*_hsc.h Judy/*.hi Judy/*.o *.hi *.o tests/*.hi tests/*.o
