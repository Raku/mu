all:
	mkdir build;cd build;cmake ..;MAKELEVEL=0 make
.PHONY : test all
test_: all
	prove -e 'valgrind -q' -r build/test
test: all
	prove --perl 'valgrind -q' -r build/test
env:
	env


