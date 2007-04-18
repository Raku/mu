#! /usr/bin/perl

use Perl6::Perldoc;
use strict;
use warnings;
use Test::More tests=>37;

my (@from_handle, @from_scalar, @from_array);

BEGIN {
    is 0+@DATA, 3  => 'Correct number of DATA blocks';

    @from_handle = <DATA>;
    @from_scalar = split /\n/, $DATA;
    @from_array  = map { split /\n/ } @DATA;

    chomp @from_handle;
    chomp @from_scalar;
    chomp @from_array;
}

sub check_all_three {
    my ($target) = @_;

    is $target, shift(@from_scalar)  =>  "'$target' from \$DATA";
    is $target, shift(@from_array)   =>  "'$target' from \@DATA";
    is $target, shift(@from_handle)  =>  "'$target' from \*DATA";
}

=DATA 1.1
1.2
1.3

=for test
this is a test

for (1..9) {
=begin Foo
=begin Bar
A comment
=end Bar
=end Foo
    ok 1 => 'Loop functioning correctly';
}

check_all_three('1.1');
check_all_three('1.2');
check_all_three('1.3');
check_all_three('2.1');
check_all_three('2.2 is data too');
check_all_three('2.3');
check_all_three('3.1');
check_all_three('');
check_all_three('3.3');

=for DATA 2.0
2.1
2.2 is data too
2.3
=config head1 :numbered
print "\n";

=END

=begin DATA 3.0
3.1

3.3
=end DATA

=head1 NAME

try.pl - [description here]

=head1
VERSION

This documentation refers to try.pl version 0.0.1

=for head1 :!numbered
USAGE

    try.pl [options]

=head1 REQUIRED ARGUMENTS

=begin nested

None

=end nested

=head1 OPTIONS

