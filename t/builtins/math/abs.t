#!/usr/bin/pugs

use v6;
require Test;

plan 40;

=pod 

Basic tests for the abs() builtin

=cut

for(0, 0.0, 1, 50, 60.0, 99.99) {
    is(abs($_), $_, "got the right absolute value for $_");
    is(ref abs($_), ref $_, "got the right data type("~ref($_)~") of absolute value for $_");
}
for(-1, -50, -60.0, -99.99) {
    is(abs($_), -$_, "got the right absolute value for $_");
    is(ref abs($_), ref $_, "got the right data type("~ref($_)~") of absolute value for $_");
}

for(0, 0.0, 1, 50, 60.0, 99.99) {
    is(abs(), $_, 'got the right absolute value for $_='~$_);
    is(ref abs(), ref $_, 'got the right data type('~ref($_)~') of absolute value for $_='~$_);
}
for(-1, -50, -60.0, -99.99) {
    is(abs(), -$_, 'got the right absolute value for $_='~$_);
    is(ref abs(), ref $_, 'got the right data type('~ref($_)~') of absolute value for $_='~$_);
}
