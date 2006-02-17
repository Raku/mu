# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 12 };
use Regexp::Parser;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $r = Regexp::Parser->new;
my $rx = '(?i:a)b(?i)c(?-i)d';

ok( $r->regex($rx) );
ok( my $w = $r->walker and 1 );
while (my ($n, $d) = $w->()) {
  chomp(my $exp = <DATA>);
  ok( join("\t", $d, $n->family, $n->type, $n->visual), $exp );
}
ok( scalar(<DATA>), "DONE\n" );

__DATA__
0	group	group	(?i:a)
1	exact	exactf	a
0	close	tail	
0	exact	exact	b
0	flags	flags	(?i)
0	exact	exactf	c
0	flags	flags	(?-i)
0	exact	exact	d
DONE
