.PHONY : prove
all: CORE

CORE: CORE/CORE.mildew.so CORE/Return.mildew.so CORE/Multi.mildew.so


CORE/CORE.mildew.so : CORE/CORE.pm
	perl mildew -Cso --empty-setting -o CORE/CORE.mildew.so CORE/CORE.pm

CORE/Return.mildew.so : CORE/Return.pm
	perl mildew -Cso --empty-setting -o CORE/Return.mildew.so CORE/Return.pm

CORE/Multi.mildew.so : CORE/Multi.pm
	perl mildew -Cso  --empty-setting -o CORE/Multi.mildew.so CORE/Multi.pm

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)

test: prove

prove:
	$(PROVE) 'perl mildew ' t/

clean:
	rm -rf lex
