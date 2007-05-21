grammar Grammar::IRC::RFC1459 {
    token message  { [ ':' <prefix> <SPACE> ]? <command> <params> <crlf> }
    token prefix   { <servername> | [ <nick> [ '!' <user> ]? [ '@' <host> ]? ] }
    token command  { <letter>+ | <number>**{3} }
    token params   { <SPACE> [ [ ':' <trailing> ] | [ <middle> <params> ] ]? }
    token middle   { <-[\x20 \x0 \x0A \x0D \:]> <-[\x20 \x0 \x0A \x0D]>* }
    token trailing { <-[\x0 \x0A \x0D]>* }
    token crlf     { \x0D \x0A }
    token SPACE    { ' '+ }
    
    token target     { <to> [ ',' <target> ]? }
    token to         {
        <channel> | [ <user> '@' <servername> ] | <nick> | <mask>
    }
    token channel    { < # & > <chstring> }
    token servername { <host> }
    token host       { # see RFC952... hmm... okay, I'll give it a shot
        <[ \x41 .. \x5A \x61 .. \x7A ]>
        <[ \x41 .. \x5A \x61 .. \x7A \x30 .. \x39 \- \. ]>+
        <[ \x41 .. \x5A \x61 .. \x7A \x30 .. \x39 ]>
            # must not end with a dot or a hyphen
        
        { 1 < $0.bytes <= 24 or fail }
    }
    token nick       { <letter> [ <letter> | <number> | <special> ]* }
    token mask       { < # & > <chstring> }
    token chstring   { <[\x0 .. \xFF] - [\x20 \c[BEL] \x0 \x0D \x0A \,]>+ }
    
    token user     { <nonwhite>+ }
    token letter   { <[\x41 .. \x5A \x61 .. \x7A]> } # A-Z a-z
    token number   { <[\x30 .. \x39]> } # 0-9
    token special  { < - [ ] \\ ` ^ { } > }
    token nonwhite { <[\x0 .. \xFF] - [\x20 \x0 \x0D \x0A]> }
}

grammar Grammar::IRC::RFC2810 is Grammar::IRC::RFC1459 { } # zero-derive

grammar Grammar::IRC::RFC2811 is Grammar::IRC::RFC2810 {
    token channel { < & # + ! > <-[\x20 \x07 \, \:]>**{ 1 .. 49 } }
        # from section 2.1 Namespace
}

grammar Grammar::IRC::RFC2812 {
    token message { [ ':' <prefix> <SPACE> ]? <command> <params>? <crlf> }
    token prefix  {
        <servername> | [ <nickname> [ [ '!' <user> ]? '@' <host> ]? ]
    }
    token command { <letter>**{ 1 .. * } | <digit>**{3} }
    token params  {
        [ [ <SPACE> <middle> ]**{ 0 .. 14 } [ <SPACE> ':' <trailing> ]? ]
      | [ [ <SPACE> <middle> ]**{ 14 } [ <SPACE> ':'? <trailing> ]? ]
    }
    token nospcrlfcl {
        <[
            \x01 .. \x09
            \x0B .. \x0C
            \x0E .. \x1F
            \x21 .. \x39
            \x3B .. \xFF
        ]>
    }
    token middle   { <nospcrlfcl> [ ':' | <nospcrlfcl> ]* }
    token trailing { [ ':' | ' ' | <nospcrlfcl> ]* }
    token SPACE    { \x20 }
    token crlf     { \x0D \x0A }
    
    token target { <nickname> | <server> }
    token msgtarget { <msgto> [ ',' <msgto> ]* }
    token msgto {
        [ <channel> | [ <user> [ '%' <host> ]? '@' <servername> ] ]
      | [ [ <user> '%' <host> ] | <targetmask> ]
      | [ <nickname> | [ <nickname> '!' <user> '@' <host> ] ]
    }
    token channel {
        [ '#' | '+' | [ '!' <channelid> ] | '&' ]
        <chanstring>
        [ ':' <chanstring> ]?
    }
    token servername { <hostname> }
    token host { <hostname> | <hostaddr> }
    token hostname {
        <shortname> [ '.' <shortname> ]*
        { $0.chars <= 63 or fail }
    }
    token shortname {
        [ <letter> | <digit> ] [ <letter> | <digit> | '-' ]*
        [ <letter> | <digit> ]*
    }
    token hostaddr { <ip4addr> | <ip6addr> }
    token ip4addr {
        <digit>**{ 1 .. 3 } '.'
        <digit>**{ 1 .. 3 } '.'
        <digit>**{ 1 .. 3 } '.'
        <digit>**{ 1 .. 3 }
    }
    token ip6addr {
        [ <hexdigit>**{ 1 .. * } [ ':' <hexdigit>**{ 1 .. * } ]**{7} ]
      | [ '0:0:0:0:0:' [ 0 | 'FFFF' ] ':' <ip4addr> ]
    }
    token nickname {
        [ <letter> | <special> ] [ <letter> | <digit> | <special> | '-' ]**{8}
    }
    token targetmask { [ '$' | '#' ] <mask> }
    token chanstring {
        [ <[ \x01 .. \x07 \x08 \x09 \x0B \x0C \x0E .. \x1F \x21 .. \x2B ]> ]
      | [ <[ \x2D .. \x39 | \x3B .. \xFF ]> ]
    }
    token channelid { [ <[ x41 .. \x5A ]> | <digit> ]**{5} }
    
    token user {
        <[ \x01 .. \x09 \x0B .. \x0C \x0E .. \x1F \x21 .. \x3F \x41 .. \xFF ]>
            **{ 1 .. * }
    }
    token key {
        <[ \x01 .. \x05 \x07 .. \x08 \x0C \x0E .. \x1F \x21 .. \x7F ]>
            **{ 1 .. 23 }
    }
    token letter { <[ \x41 .. \x5A \x61 .. \x7A ]> }
    token digit { <[ \x30 .. \x39 ]> }
    token hexdigit { <digit + [ A B C D E F ]> }
    token special { <[ \x5B .. \x60 \x7B .. \x7D ]> }
    
    token mask { [ <nowild> | <noesc> <wildone> | <noesc> <wildmany> ]* }
        # XXX not sure about the precedence here, and the RFC confuses me
    token wildone { '?' }
    token wildmany { '*' }
    token nowild { <[ \x01 .. \x29 \x2B .. \x3E \x40 .. \xFF ]> }
    token noesc { <[ \x01 .. \x5B \x5D .. \xFF ]> }
    token matchone { <[ \x01 .. \xFF ]> }
    token matchmany { <matchone>* }
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
