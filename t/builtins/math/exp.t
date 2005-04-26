#!/usr/bin/pugs

use v6;
use Test;

plan 1;

=pod 

Basic tests for the exp() builtin

=cut

# will this be the same on all machines? or should I truncate it?
# 148.413159102577   was failing on the tinderboxes.
# 148.4131591025766  So we'll see how this works (2005-04-08).
is(exp(5), 148.4131591025766, 'got the exponent of 5');
