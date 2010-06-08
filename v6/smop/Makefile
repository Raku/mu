PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)
all:
	cd m0ld;ghc --make Setup.lhs;./Setup configure --user ;./Setup build;cp dist/build/m0ld/m0ld ../m0ld_exe
	scons -Q
	cd SMOP;perl Makefile.PL;make
.PHONY : test all
test: all
	LD_LIBRARY_PATH='build/lib' $(PROVE) 'valgrind -q --leak-check=full --show-reachable=yes ' build/t
test-no-valgrind: all
	LD_LIBRARY_PATH='build/lib' $(PROVE) '' build/t
env:
	env

clean:
	rm -fr .sconsign.dblite build/ m0ld/Setup m0ld/Setup.hi m0ld/Setup.o m0ld/dist/ m0ld_exe SMOP/Makefile SMOP/SMOP.bs SMOP/SMOP.c SMOP/SMOP.o SMOP/blib/ SMOP/pm_to_blib
