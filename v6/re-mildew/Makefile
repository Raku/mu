.PHONY : prove STD

all: STD CORE modules

STD :
	cd perl6-snapshot;make

CORE: CORE/int.mildew.so CORE/CORE.mildew.so CORE/Exception.mildew.so CORE/Multi.mildew.so CORE/RoleHOW.mildew.so CORE/EXTERNAL.mildew.so CORE/Return.mildew.so CORE/Failure.mildew.so CORE/Signature.mildew.so CORE/Types.mildew.so CORE/ModuleLoader.mildew.so

modules: lib/Test.mildew.so


CORE/int.mildew.so : CORE/int.pm
	perl mildew -Cso --yeast --empty-setting -o $@ $<

CORE/%.mildew.so : CORE/%.pm
	perl mildew -Cso --yeast --target-stage 1 --empty-setting -o $@ $<

lib/%.mildew.so : lib/%.pm
	perl mildew -Cso --empty-setting -o $@ $<

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove --timer -r -e'; fi)

test: prove

prove: CORE
	$(PROVE) 'perl mildew' `cat TESTS` t/
test-yeast: CORE
	$(PROVE) 'perl mildew --yeast' `cat TESTS` t/

clean:
	rm -fr CORE.pad.store CORE/*.mildew.so lib/*.mildew.so lex/ perl6-snapshot/CORE.pad.store perl6-snapshot/STD.pm5 perl6-snapshot/STD.pmc perl6-snapshot/lex/
.PHONY : clean
