#!/usr/bin/pugs

use v6;
require Test;

require Pod::Stream::Parser;

my %events = (
    begin_document => sub { say "<HTML><BODY>" },
    end_document   => sub { say "</BODY></HTML>" },
    begin_header   => sub ($size) { print "<H$size>" },
    end_header     => sub ($size) { say "</H$size>" },    
    begin_list     => sub { say "<UL>" },
    end_list       => sub { say "</UL>" },  
    begin_item     => sub { print "<LI>" },
    end_item       => sub { say "</LI>" },    
    verbatim       => sub ($text) { say "<BLOCKQUOTE><PRE>" ~ $text ~ "</PRE></BLOCKQUOTE>"; },
    text           => sub ($text) { say $text ~ "<BR>" unless $text eq ''; },
    begin_modifier => sub ($mod) { print "<" ~ $mod ~ ">"; },
    end_modifier   => sub ($mod) { print "</" ~ $mod ~ ">"; },
    string         => sub ($str) { print $str; }         
);

parse("t/sample.pod", %events);