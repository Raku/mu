#!/usr/bin/pugs

use v6;
use Test;

# Another bug found by Limbic_Region

plan 2;

my $should_ret_empty_list1 = sub { return (); my $x = 5 };
my $should_ret_empty_list2 = sub { return () };

is (*$should_ret_empty_list1()).elems, 0, "our sub returned an empty list (1)";
is (*$should_ret_empty_list2()).elems, 0, "our sub returned an empty list (2)";
