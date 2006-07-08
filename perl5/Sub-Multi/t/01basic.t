#!/usr/bin/perl -w
use strict;
use warnings;
use Test::More tests => 4;
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
