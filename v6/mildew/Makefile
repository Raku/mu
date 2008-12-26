.PHONY : prove

PROVE := $(shell if prove --version | grep v2 > /dev/null 2>&1;then echo 'prove -r --perl'; else echo 'prove -r -e'; fi)

test: prove

prove:
	$(PROVE) 'perl mildew ' t/

clean:
	rm -rf lex
