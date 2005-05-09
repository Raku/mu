#!/usr/bin/pugs

use v6;
use Test;

use_ok('Perl::MetaAssoc');
use_ok('Perl::MetaClass');

dies_ok { Perl::MetaAssoc::new('Nothing') }, '... this dies without a Perl::MetaClass arg';

my $class = Perl::MetaClass::new('Foo');

my $assoc = Perl::MetaAssoc::new($class);
ok($assoc.instance_isa('Perl::MetaAssoc'), '... it is a MetaAssoc instance');