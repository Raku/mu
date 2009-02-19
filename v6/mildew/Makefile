.PHONY : prove
all: prelude

prelude: prelude/Prelude.mildew.so prelude/Return.mildew.so prelude/Multi.mildew.so


prelude/Prelude.mildew.so : prelude/Prelude.pm
	perl mildew -Cso --no-prelude -o prelude/Prelude.mildew.so prelude/Prelude.pm

prelude/Return.mildew.so : prelude/Return.pm
	perl mildew -Cso --no-prelude -o prelude/Return.mildew.so prelude/Return.pm

prelude/Multi.mildew.so : prelude/Multi.pm
	perl mildew -Cso  -o prelude/Multi.mildew.so prelude/Multi.pm

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)

test: prove

prove:
	$(PROVE) 'perl mildew ' t/

clean:
	rm -rf lex
