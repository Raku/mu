#!/usr/bin/pugs
use v6;

my Str %text_strings is readonly = (
    'MYAPP_HELLO' => q[Light goes on!],
    'MYAPP_GOODBYE' => q[Light goes off!],
    'MYAPP_PROMPT'
        => q[Give me a county thingy, or push that big button instead.],
    'MYAPP_RESULT'
        => q[Turn "<ORIGINAL>" upside down and get "<INVERTED>",]
           ~ q[ not "<ORIGINAL>".],
    'MYLIB_MYINV_NO_ARG' => q[Why you little ...!],
    'MYLIB_MYINV_BAD_ARG' => q["<GIVEN_VALUE>" isn't a county thingy!],
    'MYLIB_MYINV_RES_INF' => q[Don't you give me a big donut!],
);

module MyApp::L::Homer {
    sub get_text_by_key (Str $msg_key) returns Str {
        return %text_strings{$msg_key};
    }
} # module MyApp::L::Homer
