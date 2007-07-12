#!/usr/bin/perl
use KindaPerl6::Runtime::Perl5::DispatchSugar;
print "1..1\n";
my $name = sugar {_dispatch=>sub {my ($self,$method,@args) = @_;print "$method\n"}};
$name->ok;



