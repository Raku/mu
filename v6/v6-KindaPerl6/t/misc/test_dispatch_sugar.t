#!/usr/bin/perl
use KindaPerl6::Runtime::Perl5::DispatchSugar;
use Test::More tests=>2;
my $name = sugar {_dispatch=>sub {my ($self,$method,@args) = @_;return ".$method"}};
is($name->ok,".ok","sugar");
KindaPerl6::Runtime::Perl5::DispatchSugar::sugar_off();
eval {$name->not_ok};
ok $@,"sugar_off";
