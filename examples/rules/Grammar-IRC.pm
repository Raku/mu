grammar Grammar::IRC::RFC1459 {
    regex message  { [ ':' <prefix> <SPACE> ]? <command> <params> <crlf> }
    regex prefix   { <servername> | [ <nick> [ '!' <user> ]? [ '@' <host> ]? ] }
    regex command  { <letter>+ | <number>**{3} }
    regex params   { <SPACE> [ [ ':' <trailing> ] | [ <middle> <params> ] ]? }
    regex middle   { <-[\x20 \x0 \x0A \x0D \:]> <-[\x20 \x0 \x0A \x0D]>* }
    regex trailing { <-[\x0 \x0A \x0D]>* }
    regex crlf     { \x0D \x0A }
    regex SPACE    { ' '+ }
    
    regex target     { <to> [ ',' <target> ]? }
    regex to         { <channel> | [ <user> '@' <servername> ] | <nick> | <mask> }
    regex channel    { < # & > <chstring> }
    regex servername { <host> }
    regex host       { } # see RFC952... gak
    regex nick       { <letter> [ <letter> | <number> | <special> ]* }
    regex mask       { < # & > <chstring> }
    regex chstring   { <[\x0 .. \xFF] - [\x20 \c[BEL] \x0 \x0D \x0A \,]>+ }
    
    regex user     { <nonwhite>+ }
    regex letter   { <[\x41 .. \x5A \x61 .. \x7A]> }
    regex number   { <[\x30 .. \x39]> }
    regex special  { < - [ ] \\ ` ^ { } > }
    regex nonwhite { <[\x0 .. \xFF] - [\x20 \x0 \x0D \x0A]> }
}

grammar Grammar::IRC::RFC2810 is Grammar::IRC::RFC1459 { } # zero-derive

grammar Grammar::IRC::RFC2811 is Grammar::IRC::RFC2810 {
    regex channel { < & # + ! > <-[\x20 \x07 \, \:]>**{ 1 .. 49 } } # from section 2.1 Namespace
}

grammar Grammar::IRC::RFC2812 {
    regex message { [ ':' <prefix> <SPACE> ]? <command> <params>? <crlf> }
    regex prefix  { <servername> | [ <nickname> [ [ '!' <user> ]? '@' <host> ]? ] }
    regex command { <letter>**{ 1 .. * } | <digit>**{3} }
    regex params  {
        [ [ <SPACE> <middle> ]**{ 0 .. 14 } [ <SPACE> ':' <trailing> ]? ]
      | [ [ <SPACE> <middle> ]**{ 14 } [ <SPACE> ':'? <trailing> ]? ]
    }
    regex nospcrlfcl {
        <[
            \x01 .. \x09
            \x0B .. \x0C
            \x0E .. \x1F
            \x21 .. \x39
            \x3B .. \xFF
        ]>
    }
    regex middle   { <nospcrlfcl> [ ':' | <nospcrlfcl> ]* }
    regex trailing { [ ':' | ' ' | <nospcrlfcl> ]* }
    regex SPACE    { \x20 }
    regex crlf     { \x0D \x0A }
    
    regex target { <nickname> | <server> }
    regex msgtarget { <msgto> [ ',' <msgto> ]* }
    regex msgto {
        [ <channel> | [ <user> [ '%' <host> ]? '@' <servername> ] ]
      | [ [ <user> '%' <host> ] | <targetmask> ]
      | [ <nickname> | [ <nickname> '!' <user> '@' <host> ] ]
    }
    regex channel { [ '#' | '+' | [ '!' <channelid> ] | '&' ] <chanstring> [ ':' <chanstring> ]? }
    regex servername { <hostname> }
    regex host { <hostname> | <hostaddr> }
    regex hostname { <shortname> [ '.' <shortname> ]* { $0.chars <= 63 or fail } }
    regex shortname { [ <letter> | <digit> ] [ <letter> | <digit> | '-' ]* [ <letter> | <digit> ]* }
    regex hostaddr { <ip4addr> | <ip6addr> }
    regex ip4addr { <digit>**{ 1 .. 3 } '.' <digit>**{ 1 .. 3 } '.' <digit>**{ 1 .. 3 } '.' <digit>**{ 1 .. 3 } }
    regex ip6addr {
        [ <hexdigit>**{ 1 .. * } [ ':' <hexdigit>**{ 1 .. * } ]**{7} ]
      | [ '0:0:0:0:0:' [ 0 | 'FFFF' ] ':' <ip4addr> ]
    }
    regex nickname { [ <letter> | <special> ] [ <letter> | <digit> | <special> | '-' ]**{8} }
    regex targetmask { [ '$' | '#' ] <mask> }
    regex chanstring {
        [ <[ \x01 .. \x07 \x08 \x09 \x0B \x0C \x0E .. \x1F \x21 .. \x2B ]> ]
      | [ <[ \x2D .. \x39 | \x3B .. \xFF ]> ]
    }
    regex channelid { [ <[ x41 .. \x5A ]> | <digit> ]**{5} }
    
    regex user { <[ \x01 .. \x09 \x0B .. \x0C \x0E .. \x1F \x21 .. \x3F \x41 .. \xFF ]>**{ 1 .. * } }
    regex key { <[ \x01 .. \x05 \x07 .. \x08 \x0C \x0E .. \x1F \x21 .. \x7F ]>**{ 1 .. 23 } }
    regex letter { <[ \x41 .. \x5A \x61 .. \x7A ]> }
    regex digit { <[ \x30 .. \x39 ]> }
    regex hexdigit { <digit + [ A B C D E F ]> }
    regex special { <[ \x5B .. \x60 \x7B .. \x7D ]> }
    
    regex mask { [ <nowild> | <noesc> <wildone> | <noesc> <wildmany> ]* }
        # XXX not sure about the precedence here, and the RFC confuses me
    regex wildone { '?' }
    regex wildmany { '*' }
    regex nowild { <[ \x01 .. \x29 \x2B .. \x3E \x40 .. \xFF ]> }
    regex noesc { <[ \x01 .. \x5B \x5D .. \xFF ]> }
    regex matchone { <[ \x01 .. \xFF ]> }
    regex matchmany { <matchone>* }
}

grammar Grammar::IRC::RFC2813 is Grammar::IRC::RFC2812 { } # zero-derive
grammar Grammar::IRC is Grammar::IRC::RFC2813 { } # zero-derive

__END__
=NAME
Grammar::IRC - Grammar for parsing IRC protocol

=begin SYNOPSIS
=begin code

use Grammar::IRC;

for =$sock -> $line {
    $line ~~ /^ <message> $/;
    ...
}

=end code
=end SYNOPSIS

=DESCRIPTION
Grammar::IRC consists of a set of grammars for parsing the IRC protocol. A
grammar each is included for parsing the protocol according to RFC1459, RFC2810,
RFC2811, RFC2812 and RFC2813. Of these, only RFC1459, RFC2811 and RFC2812 are
distinct. The others are included for completeness. The grammar
C<Grammar::IRC> is a zero-derivation of C<Grammar::IRC::RFC2813>. You may,
however, import any of the other grammars as you wish.

=AUTHOR
Aankhen, C<< <aankhen at cpan.org> >>.

=begin SEEALSO
The various RFCs:

=item L<RFC1459|http://tools.ietf.org/html/rfc1459>

=item L<RFC2810|http://tools.ietf.org/html/rfc2810>

=item L<RFC2811|http://tools.ietf.org/html/rfc2811>

=item L<RFC2812|http://tools.ietf.org/html/rfc2812>

=item L<RFC2813|http://tools.ietf.org/html/rfc2813>
=end SEEALSO

=begin COPYRIGHT AND LICENCE
Copyright 2007 Aankhen, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.
=end COPYRIGHT AND LICENCE
