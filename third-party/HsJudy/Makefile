

LDLIBS= -L/usr/local/lib -lJudy
CC=ghc 

CFLAGS= -O

CHEADERS= Judy/Private_hsc.h
HSFLAGS= $(HSF) -O2 -fffi -I/usr/local/include -fglasgow-exts -I. -IJudy '-\#include Judy/Private_hsc.h' $(LDLIBS)

%.hi: %.o
	@:
%_hsc.c: %.hs
	@:
%_hsc.h: %.hs
	@:

%.hs: %.hsc
	hsc2hs -C "-I/usr/local/include" -C "$(CFLAGS)" -o $@ $<


all: try testbs testjl testjsl

Judy_hsc.c: Judy_hsc.h

testbs: tests/TestBS.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

testjl: tests/TestJL.hs Judy/Private.hs Judy/Private_hsc.o 
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

testjsl: tests/TestJSL.hs Judy/Private.hs Judy/Private_hsc.o 
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

try: Try.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

trybench: TryBench.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

.PHONY: test clean

test: testbs testjl testjsl
	@echo
	@./testbs
	@echo
	@./testjl
	@echo
	@./testjsl
	@echo

clean:
	rm -f try testbs testjl testjsl Judy/Private.hs Judy/*_hsc.c Judy/*_hsc.h Judy/*.hi Judy/*.o *.hi *.o tests/*.hi tests/*.o
