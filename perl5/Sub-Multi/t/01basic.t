#!/usr/bin/perl -w
use strict;
use warnings;
use Test::More tests => 8;
use Sub::Multi;
use Test::Exception;

sub foo1 { return 1 }
Data::Bind->sub_signature(\&foo1, { var => '$title' });

sub foo2 { return 2 }
Data::Bind->sub_signature(\&foo2, { var => '$bzz' });

*foo = Sub::Multi->new(\&foo1, \&foo2);

is( foo([], { title => \'orz'}), 1);
is( foo([], { bzz => \'orz'}), 2);

throws_ok {
    warn foo([\'orz'], {});
} qr/ambiguous/;

throws_ok {
    warn foo([], {});
} qr/vapour/;

sub bar1 { return 1 }
Data::Bind->sub_signature(\&bar1, { var => '$title' });
BEGIN { Sub::Multi->add_multi('bar', \&bar1 ) }

sub bar2 { return 2 }
Data::Bind->sub_signature(\&bar2, { var => '$bzz' });
BEGIN { Sub::Multi->add_multi('bar', \&bar2 ) }

is( bar([], { title => \'orz'}), 1);
is( bar([], { bzz => \'orz'}), 2);

throws_ok {
    warn bar([\'orz'], {});
} qr/ambiguous/;

throws_ok {
    warn bar([], {});
} qr/vapour/;
