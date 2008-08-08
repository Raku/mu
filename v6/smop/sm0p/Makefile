all: sm0p5.pm
check:
sm0p5.pm: sm0p.pm
	touch ../test/*.p6 ../test/*.sm0p ../src/*.sm0p
	rm -fr lex/
	perl strip_out_actions sm0p.pm
	cd ../../../src/perl6/;perl gimme5 ../../v6/smop/sm0p/striped > ../../v6/smop/sm0p/sm0p5.pm
