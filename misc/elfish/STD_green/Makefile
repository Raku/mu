
STDBLUE=../STD_blue/STD_blue_run
FAKEBLUE=./STD_blue_run
STDPM=../../../src/perl6/STD.pm
STDTREE=./tree
ELFBLUE=../STD_blue/elfblue

tst:
	${ELFBLUE} -v -s STD.pm

setup:
	cp ${STDPM} .
	${STDBLUE} --format=p5a ${STDPM} > ${STDTREE}

setup_blue:
	rm -rf lex
	cd ../STD_blue; make
