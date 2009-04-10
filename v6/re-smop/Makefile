PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)
all:
	cd m0ld;ghc --make Setup.lhs;./Setup configure --user ;./Setup build;cp dist/build/m0ld/m0ld ../m0ld_exe
	scons -Q
.PHONY : test all
test: all
	LD_LIBRARY_PATH='build/lib' $(PROVE) 'valgrind -q --leak-check=full' build/t
env:
	env

clean:
	scons --clean
