package TestChunks;
use strict;
# use warnings;
use base 'Exporter';
our @EXPORT = qw(plan is ok like is_deeply fail chunks);
use Test::More;
use Carp;

BEGIN {
    $SIG{__DIE__} = sub {
        Carp::confess(@_);
    };
}

sub chunks {
    my $data = get_data();
    my @chunks = ($data =~ /^(===.*?(?=^===|\z))/msg);
    my @tests;
    for my $chunk (@chunks) {
        my $test = {};
        $chunk =~ s/\A===[ \t]*(.*)\s+// or die;
        my $description = $1 || 'No test description';
        my @parts = split /^==>\s+(\w+)\s+/m, $chunk;
        shift @parts;
        %$test = @parts;
        $test->{description} = $description;
        return @tests = $test if defined $test->{only};
        push @tests, $test;
    }
    return @tests;
}

my $data;
sub get_data {
    no warnings;
    $data = $data || do { package main; local $/; <DATA> };
}

1;
