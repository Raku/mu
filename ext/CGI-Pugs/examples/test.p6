#!/usr/bin/pugs

use v6;
require CGI::Pugs-0.0.1;

say header;

if (param()) {
    for param() -> $key {
        say param($key) ~ "<BR>";
    }
}
else {
    say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
}