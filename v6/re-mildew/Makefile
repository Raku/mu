.PHONY : prove STD

all: STD CORE

STD :
	cd perl6-snapshot;make

CORE: CORE/CORE.mildew.so CORE/Multi.mildew.so CORE/RoleHOW.mildew.so CORE/EXTERNAL.mildew.so CORE/Return.mildew.so



CORE/CORE.mildew.so : CORE/CORE.pm
	perl mildew -Cso --empty-setting -o CORE/CORE.mildew.so CORE/CORE.pm

CORE/Return.mildew.so : CORE/Return.pm
	perl mildew -Cso --empty-setting -o CORE/Return.mildew.so CORE/Return.pm

CORE/Multi.mildew.so : CORE/Multi.pm
	perl mildew -Cso  --empty-setting -o CORE/Multi.mildew.so CORE/Multi.pm

CORE/RoleHOW.mildew.so : CORE/RoleHOW.pm
	perl mildew -Cso  --empty-setting -o CORE/RoleHOW.mildew.so CORE/RoleHOW.pm

CORE/EXTERNAL.mildew.so : CORE/EXTERNAL.pm
	perl mildew -Cso  --empty-setting -o CORE/EXTERNAL.mildew.so CORE/EXTERNAL.pm

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)

test: prove

prove: CORE
	$(PROVE) 'perl mildew --valgrind ' `cat TESTS` t/

clean:
	rm -rf lex
