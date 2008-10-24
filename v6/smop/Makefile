all:
	mkdir build;cd build;cmake ..;MAKELEVEL=0 make
.PHONY : test all
test: all

	prove -r -e 'valgrind -q' build/test
	#perl -MTest::Harness -e '@tests=<build/test/*.t>;$$Test::Harness::switches="-e \"system(shift)\"";runtests(@tests)'
env:
	env


