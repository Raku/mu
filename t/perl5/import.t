#!/usr/bin/pugs

use v6;
use Test;
plan 1;

=pod

P5 module import test

=cut

unless try({ eval("1", :lang<perl5>) }) {
    skip_rest;
    exit;
}

eval q[

use perl5:Digest::MD5;
ok(md5("foo\n") eq "d3b07384d113edec49eaa6238ad5ff00", "import p5 module");

];
