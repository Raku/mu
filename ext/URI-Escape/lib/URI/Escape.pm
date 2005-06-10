#!/usr/bin/pugs
use v6;

module URI::Escape-0.0.1;

our %escapes;

for 0..255 -> $char {
    %escapes{chr($char)} = $char.as('%%%02X');
}

# XXX need to handle the Rule case -- must check that $0 is being set
#multi sub uri_escape (Str $string is copy, Rule $unsafe) returns Str is export(:DEFAULT) {
#    ...
#}

multi sub uri_escape (Str $string is copy, Str $unsafe, Bool +$negate) returns Str is export(:DEFAULT) {
    my $pattern;
    
    $pattern = ($negate) ?? '([^$unsafe])' :: '([$unsafe])';
    
    $string ~~ s:P5:g/$pattern/{ %escapes{$0} || fail_hi($0) }/;
    
    return $string;
}

multi sub uri_escape (Str $string is copy) returns Str is export(:DEFAULT) {
    $string = uri_escape($string, "A-Za-z0-9\-_.!~*'()", negate => bool::true);
    
    return $string;
}

# XXX need Encode for this
multi sub uri_escape_utf8 (Str $string is copy, Rule $unsafe) returns Str is export(:DEFAULT) {
    ...
}

multi sub uri_escape_utf8 (Str $string is copy, Str $unsafe) returns Str is export(:DEFAULT) {
    ...
}

multi sub uri_escape_utf8 (Str $string is copy) returns Str is export(:DEFAULT) {
    ...
}

sub uri_unescape (*@str is copy) returns Str is export(:DEFAULT) {
    ...
}

sub fail_hi (Str $char) {
    die sprintf("Can't escape \\x{%04X}, try uri_escape_utf8() instead", ord $char);
}

1;
