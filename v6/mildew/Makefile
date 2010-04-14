.PHONY : prove 

all: CORE CORE-js modules


CORE: CORE/int.mildew.so CORE/CORE.mildew.so CORE/Exception.mildew.so CORE/Multi.mildew.so CORE/RoleHOW.mildew.so CORE/EXTERNAL.mildew.so CORE/Return.mildew.so CORE/Failure.mildew.so CORE/Signature.mildew.so CORE/Types.mildew.so CORE/ModuleLoader.mildew.so CORE/Eval.mildew.so 

CORE-js: js-compiled/CORE2.js js-compiled/molds.js js-compiled/RoleHOW.js js-compiled/Exception.js js-compiled/Failure.js js-compiled/Multi.js js-compiled/Signature.js js-compiled/Types.js js-compiled/int.js js-compiled/ModuleLoader.js lib/Test.mildew.js

modules: lib/Test.mildew.so


CORE/int.mildew.so : CORE/int.pm
	perl mildew -Cso --yeast --empty-setting -o $@ $<

CORE/Eval.mildew.so : CORE/Eval.pm
	perl mildew -Cso --yeast --empty-setting -o $@ $<

CORE/%.mildew.so : CORE/%.pm
	perl mildew -Cso --yeast --target-stage 1 --empty-setting -o $@ $<

js-compiled/molds.js : js/molds
	mkdir -p js-compiled
	perl write_molds $< > $@
js-compiled/%.js : js/%.pm
	mkdir -p js-compiled
	perl mildew --target-stage 1 -Cjs -o $@ $<

js-compiled/%.js : CORE/%.pm
	mkdir -p js-compiled
	perl mildew --target-stage 1 -Cjs -o $@ $<

js-compiled/int.js : CORE/int.pm
	mkdir -p js-compiled
	perl mildew -Cjs -o $@ $<

lib/%.mildew.js : lib/%.pm
	perl mildew -Cjs-lib -o $@ $<

lib/%.mildew.so : lib/%.pm
	perl mildew -Cso --empty-setting -o $@ $<

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove --timer --state hot,save -r -e'; fi)

test: prove

prove: CORE
	$(PROVE) 'perl mildew' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

test-js: CORE-js
	$(PROVE) 'perl mildew -Bjs' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS-js)

test-ssa: CORE
	$(PROVE) 'perl mildew -Bssa' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

test-bread: CORE
	$(PROVE) 'perl bread' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

test-optC: CORE
	$(PROVE) 'perl mildew -BoptC' t-type-inference $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

test-js-t: CORE-js
	$(PROVE) 'perl mildew -Bjs' t

test-yeast: CORE
	$(PROVE) 'perl mildew --yeast' $(shell xargs -n 1 perl ../../t/spec/fudge --keep-exit-code mildew < TESTS) t

clean:
	rm -fr CORE.pad.store CORE/*.mildew.so lib/*.mildew.so lex/
.PHONY : clean

# List all targets with brief descriptions.
help:
	@echo
	@echo 'In pugs/v6/mildew you can make these targets:'
	@echo
	@echo 'all (default)   builds mildew'
	@echo 'test            proves mildew with default (smop) backend'
	@echo 'test-js         proves mildew with JavaScript (V8) backend'
	@echo 'clean           removes generated files'
	@echo
