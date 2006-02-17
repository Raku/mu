# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 52 };
use Regexp::Parser;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $r = Regexp::Parser->new;
my $rx = '^a+b*?c{5,}d{3}$';

ok( $r->regex($rx) );

for my $arg (-1, 0, 1, 2) {
  ok( my $w = $r->walker($arg) and 1 );
  ok( $w->(-depth) == $arg );
  while (my ($n, $d) = $w->()) {
    chomp(my $exp = <DATA>);
    ok( join("\t", $d, $n->family, $n->type, $n->visual), $exp );
  }
  ok( scalar(<DATA>), "DONE\n" );
}

__DATA__
0	anchor	bol	^
0	quant	plus	a+
1	exact	exact	a
0	minmod	minmod	b*?
1	quant	star	b*
2	exact	exact	b
0	quant	curly	c{5,}
1	exact	exact	c
0	quant	curly	d{3}
1	exact	exact	d
0	anchor	eol	$
DONE
0	anchor	bol	^
0	quant	plus	a+
0	minmod	minmod	b*?
0	quant	curly	c{5,}
0	quant	curly	d{3}
0	anchor	eol	$
DONE
0	anchor	bol	^
0	quant	plus	a+
1	exact	exact	a
0	minmod	minmod	b*?
1	quant	star	b*
0	quant	curly	c{5,}
1	exact	exact	c
0	quant	curly	d{3}
1	exact	exact	d
0	anchor	eol	$
DONE
0	anchor	bol	^
0	quant	plus	a+
1	exact	exact	a
0	minmod	minmod	b*?
1	quant	star	b*
2	exact	exact	b
0	quant	curly	c{5,}
1	exact	exact	c
0	quant	curly	d{3}
1	exact	exact	d
0	anchor	eol	$
DONE

