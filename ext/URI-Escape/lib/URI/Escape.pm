#!/usr/bin/pugs
use v6;

module URI::Escape-0.0.1;

our %escapes;
my %subst;

for 0..255 -> $char {
    %escapes{chr($char)} = sprintf('%%%02X', $char);
}

multi sub uri_escape (Str $string is copy, Rule $unsafe) returns Str is export {
    ...
}

multi sub uri_escape (Str $string is copy, Str $unsafe) returns Str is export {
    ...
}

multi sub uri_escape (Str $string is copy) returns Str is export {
    ...
}

multi sub uri_escape_utf8 (Str $string is copy, Rule $unsafe) returns Str is export {
    ...
}

multi sub uri_escape_utf8 (Str $string is copy, Str $unsafe) returns Str is export {
    ...
}

multi sub uri_escape_utf8 (Str $string is copy) returns Str is export {
    ...
}

sub uri_unescape (Str $string is copy) returns Str is export {
    ...
}

sub fail_hi (Str $char) {
    ...
}