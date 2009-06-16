.PHONY : prove STD

all: STD CORE

STD :
	cd perl6-snapshot;make

CORE: CORE/CORE.mildew.so CORE/Exception.mildew.so CORE/Multi.mildew.so CORE/RoleHOW.mildew.so CORE/EXTERNAL.mildew.so CORE/Return.mildew.so CORE/Failure.mildew.so CORE/Signature.mildew.so CORE/Types.mildew.so



CORE/%.mildew.so : CORE/%.pm
	perl mildew -Cso --target-stage 1 --empty-setting -o $@ $<

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)

test: prove

prove: CORE
	$(PROVE) 'perl mildew --valgrind ' `cat TESTS` t/
test-no-valgrind: CORE
	$(PROVE) 'perl mildew' `cat TESTS` t/

clean:
	rm -rf lex
