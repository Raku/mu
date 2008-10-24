all:
	mkdir build;cd build;cmake ..;MAKELEVEL=0 make
.PHONY : test all
test: all
	prove --perl 'valgrind -q' -r build/test
	#perl -MTest::Harness -e '@tests=<build/test/*.t>;$$Test::Harness::switches="-e \"system(q<valgrind -q >.shift)\"";runtests(@tests)'
env:
	env


