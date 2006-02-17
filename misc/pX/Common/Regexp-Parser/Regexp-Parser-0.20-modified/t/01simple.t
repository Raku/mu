# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 14 };
use Regexp::Parser;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $r = Regexp::Parser->new;
my $rx = '^a+b*?c{5,}$';

ok( $r->regex($rx) );

# for this regex, it's ok
# it won't necessarily ALWAYS be
ok( $r->visual, $rx );

ok( "aaabbbcccccc" =~ $r->qr );
ok( "aaabbbccccc"  =~ $r->qr );
ok( "aaabbbcccc"   !~ $r->qr );

ok( "aaabbbccccc" =~ $r->qr );
ok( "aaabbccccc"  =~ $r->qr );
ok( "aaabccccc"   =~ $r->qr );
ok( "aaaccccc"    =~ $r->qr );

ok( "aaabbbccccc" =~ $r->qr );
ok( "aabbbccccc"  =~ $r->qr );
ok( "abbbccccc"   =~ $r->qr );
ok( "bbbccccc"    !~ $r->qr );
