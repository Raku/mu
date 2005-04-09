#!/usr/bin/pugs

use v6;
require CGI;

print header;

if (param()) {
    for param() -> $key {
        say $key ~ " => " ~ param($key) ~ "<BR>";
    }
}
else {
    say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
}