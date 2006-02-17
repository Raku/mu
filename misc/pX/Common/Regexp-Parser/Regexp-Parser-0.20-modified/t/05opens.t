# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 8 };

use Regexp::Parser;
ok( 1 );

my $r = Regexp::Parser->new;
my $rx = '^(a(bc)+(d?))((f)+)$';
ok( $r->regex($rx) );

for (@{ $r->captures }) {
  chomp(my $exp = <DATA>);
  ok( join("\t", $_->nparen, $_->visual), $exp );
}

ok( scalar(<DATA>), "DONE\n" );

__DATA__
1	(a(bc)+(d?))
2	(bc)
3	(d?)
4	((f)+)
5	(f)
DONE
