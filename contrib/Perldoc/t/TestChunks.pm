package TestChunks;
use strict;
use warnings;
use base 'Exporter';
our @EXPORT = qw(
    plan is ok like is_deeply fail 
    data_file chunks
);
use Test::More;
use Carp;

BEGIN {
    $SIG{__DIE__} = sub {
        Carp::confess(@_);
    };
}

sub chunks {
    my $data = get_data();
    my @chunks = ($data =~ /^(==\(.*?(?=^==\(|\z))/msg);
    my @tests;
    for my $chunk (@chunks) {
        my $test = {};
        $chunk =~ s/\A==\([ \t]*(.*)\s+// or die;
        my $description = $1 || 'No test description';
        my @parts = split /^==>\s+(\w+)\s+/m, $chunk;
        shift @parts;
        %$test = @parts;
        for (keys %$test) {
            $test->{$_} ||= '';
        }
        $test->{description} = $description;
        return @tests = $test if defined $test->{ONLY};
        next if defined $test->{SKIP};
        push @tests, $test;
    }
    return @tests;
}

my $data_file;
sub data_file {
    $data_file = shift;
}

my $data;
sub get_data {
    return $data if $data;
    no warnings;
    local $/;
    if ($data_file) {
        open FILE, $data_file or die $!;
        $data = <FILE>;
        close FILE;
    }
    else {    
        $data = do { package main; local $/; <DATA> };
    }
    return $data;
}

1;
