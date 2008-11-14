PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)
all:
	mkdir build;cd build;cmake ..;MAKELEVEL=0 make
.PHONY : test all
test: all
	$(PROVE) 'valgrind -q' build/test
env:
	env


