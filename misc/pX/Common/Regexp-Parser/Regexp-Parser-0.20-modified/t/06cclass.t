# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 3 };

use Regexp::Parser;
ok( 1 );

my $r = Regexp::Parser->new;
my $rx = '[\f\b\a]+';
ok( $r->regex($rx) );
ok( $r->visual, '[\f\b\a]+' );

use Data::Dumper; $Data::Dumper::Indent = 1; print Dumper($r->root);
