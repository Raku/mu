PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)
all:
	scons -Q
.PHONY : test all
test: all
	LD_LIBRARY_PATH='build' $(PROVE) 'valgrind -q' build-tests
env:
	env


