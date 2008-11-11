.PHONY : test

test: clean prove

prove:
	prove -r -e 'perl mildew --exec --file' t/

test_: clean prove_

prove_:
	prove -r --perl 'perl mildew --exec --file' t/

clean:
	rm -rf lex
