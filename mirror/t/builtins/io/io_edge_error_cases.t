#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

Some edge and error cases for I/O

=cut

# deal with non-existant files

ok(!defined(open("file_which_does_not_exist")), 'open() on non-existant file returns undef');

open("create_this_file", :w);
ok(-e 'create_this_file', 'writing to a non-existant file creates it');
unlink('create_this_file');

open("create_this_file2", :w);
ok(-e 'create_this_file2', 'appending to a non-existant file creates it');
unlink('create_this_file2');
