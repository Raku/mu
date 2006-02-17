# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 95 };
use Regexp::Parser;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $r = Regexp::Parser->new;

my @good_rx = (
  q{(?(?=a|b)c|d)},
  q{(?(?<=a|b)c|d)},
  q{(?(?!a|b)c|d)},
  q{(?(?<!a|b)c|d)},
  q{(?(?{1})c|d)},
  q{(?(1)c|d)},
);

my @bad_rx = (
  q{(?(??{bad})c|d)},
  q{(?(?p{bad})c|d)},
  q{(?(?>bad)c|d)},
  q{(?(?:bad)c|d)},
  q{(?(?i)c|d)},
  q{(?(?#bad)c|d)},
  q{(?(BAD)c|d)},
  q{(?(1BAD)c|d)},
  q{(?()c|d)},
  q{(?((?=bad))c|d)},
  q{(?(?=bad)c|d|e)},
);

my @not_bad = (
  q{(?(?=a)(b|c)(d|e))},
  q{(?(?=a)(?(?=b)c|d))},
  q{(?(?=a)(?(?=b)c|d)|(?(?=e)f|g))},
);

for my $rx (@good_rx) {
  ok( $r->regex($rx) );
  ok( my $w = $r->walker and 1 );
  while (my ($n, $d) = $w->()) {
    chomp(my $exp = <DATA>);
    ok( join("\t", $d, $n->family, $n->type, $n->visual), $exp );
  }
  ok( scalar(<DATA>), "DONE\n" );
}

for my $rx (@bad_rx) { ok( !$r->regex($rx) ) }
for my $rx (@not_bad) { ok( $r->regex($rx) ) }

__DATA__
0	assertion	ifthen	(?(?=a|b)c|d)
1	assertion	ifmatch	(?=a|b)
2	branch	branch	a|b
3	exact	exact	a
2	branch	branch	
3	exact	exact	b
1	close	tail	
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
0	assertion	ifthen	(?(?<=a|b)c|d)
1	assertion	ifmatch	(?<=a|b)
2	branch	branch	a|b
3	exact	exact	a
2	branch	branch	
3	exact	exact	b
1	close	tail	
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
0	assertion	ifthen	(?(?!a|b)c|d)
1	assertion	unlessm	(?!a|b)
2	branch	branch	a|b
3	exact	exact	a
2	branch	branch	
3	exact	exact	b
1	close	tail	
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
0	assertion	ifthen	(?(?<!a|b)c|d)
1	assertion	unlessm	(?<!a|b)
2	branch	branch	a|b
3	exact	exact	a
2	branch	branch	
3	exact	exact	b
1	close	tail	
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
0	assertion	ifthen	(?(?{1})c|d)
1	assertion	eval	(?{1})
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
0	assertion	ifthen	(?(1)c|d)
1	groupp	groupp1	(1)
1	branch	branch	c|d
2	exact	exact	c
1	branch	branch	
2	exact	exact	d
0	close	tail	
DONE
