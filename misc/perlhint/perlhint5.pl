#!/usr/bin/perl
use warnings;
use strict;
use Perlhint::Parse;
use Data::Dumper;

my $parser = Perlhint::Parse->new({filename => 'data/perl-6.en.dat'});

print Dumper($parser->records);
