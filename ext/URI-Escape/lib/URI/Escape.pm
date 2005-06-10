#!/usr/bin/pugs
use v6;

module URI::Escape-0.0.1;

our %escapes;

for 0..255 -> $char {
    %escapes{chr($char)} = sprintf('%%%02X', $char);
}

multi sub uri_escape (Str $string, Rule $unsafe) {
    ...
}

multi sub uri_escape (Str $string) {
    ...
}

multi sub uri_escape_utf8 (Str $string, Rule $unsafe) {
    ...
}

multi sub uri_escape_utf8 (Str $string) {
    ...
}

sub uri_unescape (Str $string) {
    ...
}

sub fail_hi (Str $char) {
    ...
}