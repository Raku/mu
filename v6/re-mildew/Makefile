.PHONY : prove STD

all: STD CORE CORE-js modules

STD :
	cd perl6-snapshot;make

CORE: CORE/int.mildew.so CORE/CORE.mildew.so CORE/Exception.mildew.so CORE/Multi.mildew.so CORE/RoleHOW.mildew.so CORE/EXTERNAL.mildew.so CORE/Return.mildew.so CORE/Failure.mildew.so CORE/Signature.mildew.so CORE/Types.mildew.so CORE/ModuleLoader.mildew.so CORE/Eval.mildew.so CORE/ClassHOW.mildew.so 

CORE-js: js/CORE2.js js/molds.js js/RoleHOW.js js/Exception.js

modules: lib/Test.mildew.so


CORE/int.mildew.so : CORE/int.pm
	perl mildew -Cso --yeast --empty-setting -o $@ $<

CORE/Eval.mildew.so : CORE/Eval.pm
	perl mildew -Cso --yeast --empty-setting -o $@ $<

CORE/%.mildew.so : CORE/%.pm
	perl mildew -Cso --yeast --target-stage 1 --empty-setting -o $@ $<

js/molds.js : js/molds
	perl write_molds js/molds > js/molds.js
js/%.js : js/%.pm
	perl mildew -Cjs -o $@ $<

js/%.js : CORE/%.pm
	perl mildew -Cjs -o $@ $<

lib/%.mildew.so : lib/%.pm
	perl mildew -Cso --empty-setting -o $@ $<

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove --timer -r -e'; fi)

test: prove

prove: CORE
	$(PROVE) 'perl mildew' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

test-js: CORE-js
	$(PROVE) 'perl mildew -Bjs' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS-js)

test-js-t: CORE-js
	$(PROVE) 'perl mildew -Bjs' t

test-yeast: CORE
	$(PROVE) 'perl mildew --yeast' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

clean:
	rm -fr CORE.pad.store CORE/*.mildew.so lib/*.mildew.so lex/ perl6-snapshot/CORE.pad.store perl6-snapshot/STD.pm5 perl6-snapshot/STD.pmc perl6-snapshot/lex/
.PHONY : clean
