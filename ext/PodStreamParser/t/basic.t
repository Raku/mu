#!/usr/bin/pugs

use v6;
require Test;

require Pod::Stream::Parser;

my %events = (
    start_document => { say "<HTML><BODY>" },
    end_document   => { say "</BODY></HTML>" },
    start_header   => -> ($size) { print "<H$size>" },
    end_header     => -> ($size) { say "</H$size>" },    
    start_list     => { say "<UL>" },
    end_list       => { say "</UL>" },  
    start_item     => { print "<LI>" },
    end_item       => { say "</LI>" },    
    verbatim       => -> ($text) { say "<BLOCKQUOTE><PRE>" ~ $text ~ "</PRE></BLOCKQUOTE>"; },
    start_modifier => -> ($mod) { print "<" ~ $mod ~ ">"; },
    end_modifier   => -> ($mod) { print "</" ~ $mod ~ ">"; },
    string         => -> ($str) { print $str; }
);

parse("t/sample.pod", %events);