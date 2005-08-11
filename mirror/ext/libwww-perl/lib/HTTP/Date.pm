#!/usr/bin/pugs
use v6;

module HTTP::Date {
    sub time2str (?$time = time)  is export(:MANDATORY) { ... }
    sub str2time ($str, ?$zone)   is export(:MANDATORY) { ... }
    
    sub parse_date ($str)             is export(:DEFAULT) { ... }
    sub time_to_iso (?$time = time)   is export(:DEFAULT) { ... }
    sub time_to_isoz (?$time = time)  is export(:DEFAULT) { ... }
}
