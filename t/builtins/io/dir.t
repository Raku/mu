#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

opendir/readdir support

=cut

my IO $dir;
eval_ok 'opendir( $dir, "." )', "opendir worked", :todo<feature>;

my @files;
eval_ok '@files = readdir($dir)', "seems readdir worked too", :todo<feature>;

ok @files.elems > 0, "readdir really worked", :todo<feature>;
