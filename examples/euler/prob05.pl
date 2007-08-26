#!/usr/bin/env pugs

use v6;

for 1..* -> $i {
    if $i % all(1..20) == 0 {
        say $i;
        last;
    }
}
