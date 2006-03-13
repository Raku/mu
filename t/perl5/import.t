#!/usr/bin/pugs

use v6;
use Test;

=pod

P5 module import test

=cut

use perl5:Digest::MD5;
my &md5 := Digest::MD5.can("md5_hex"); 

plan 1;

ok(md5("foo\n") eq "d3b07384d113edec49eaa6238ad5ff00", "import p5 module");
