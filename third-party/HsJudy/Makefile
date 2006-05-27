

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


all: try

Judy_hsc.c: Judy_hsc.h


try: Try.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@

trybench: TryBench.hs Judy/Private.hs Judy/BitSet.hs Judy/Private_hsc.o
	ghc $(HSFLAGS) --make $< Judy/Private_hsc.o -o $@
