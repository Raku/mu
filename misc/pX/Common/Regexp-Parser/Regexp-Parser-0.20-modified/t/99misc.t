# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 4 };
use Regexp::Parser;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $r = Regexp::Parser->new;

ok( $r->force_object(prop => "alpha", 0)->visual eq '\p{alpha}' );
ok( $r->regex('\N{LATIN SMALL LETTER R}') );
ok( $r->root->[0]->data, "r" );

