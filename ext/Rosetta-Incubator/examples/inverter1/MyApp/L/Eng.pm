#!/usr/bin/pugs
use v6;

###########################################################################
###########################################################################

my Str %text_strings is readonly = (
    'MYAPP_HELLO' => q[Welcome to MyApp.],
    'MYAPP_GOODBYE' => q[Goodbye!],
    'MYAPP_PROMPT'
        => q[Enter a number to be inverted, or press ENTER to quit.],
    'MYAPP_RESULT' => q[The inverse of "<ORIGINAL>" is "<INVERTED>".],
);

module MyApp::L::Eng {
    sub get_text_by_key (Str $msg_key!) returns Str {
        return %text_strings{$msg_key};
    }
} # module MyApp::L::Eng

###########################################################################
###########################################################################
