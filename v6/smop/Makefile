all:
	mkdir build;cd build;cmake ..;make
.PHONY : test all
test: all
	perl -MTest::Harness -e '@tests=<build/test/*.t>;$$Test::Harness::switches="-e \"system(shift)\"";runtests(@tests)'


