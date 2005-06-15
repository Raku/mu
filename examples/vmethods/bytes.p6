#!/usr/bin/pugs
#
# based on Ruby's ActiveSupport 1.0.4
#

use v6;

multi sub bytes (Int $value:) {
    $value;
}

our &byte:=&bytes;

multi sub kilobytes (Int $value:) {
    $value * 1024;
}

our &kilobyte:=&kilobytes;

multi sub megabytes (Int $value:) {
    $value * 1024.kilobytes;
}

our &megabyte:=&megabytes;

multi sub gigabytes (Int $value:) {
    $value * 1024.megabytes;
}

our &gigabyte:=&gigabytes;

multi sub terabytes (Int $value:) {
    $value * 1024.gigabytes;
}

our &terabyte:=&terabytes;

say "5 bytes is " ~ 5.bytes ~ " bytes.";
say "5 kilobytes is " ~ 5.kilobytes ~ " bytes.";
say "1 megabyte is " ~ 1.megabyte ~ " bytes.";

