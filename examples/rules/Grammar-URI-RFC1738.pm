grammar Grammar::URI::RFC1738 {
    # URL schemeparts for ip-based protocols
    token ip-schemepart { '//' <login> [ '/' <urlpart> ]? }
    
    token login       { [ <user> [ ':' <password> ]? '@' ]? <hostport> }
    token hostport    { <host> [ ':' <port> ]? }
    token host        { <hostname> | <hostnumber> }
    token hostname    { [ <domainlabel> '.' ]* <toplabel> }
    token domainlabel {
        <alphadigit>
      | [ <alphadigit> [ <alphadigit> | '-' ]* <alphadigit> ]
    }
    token toplabel    {
        <alpha>
      | [ <alpha> [ <alphadigit> | '-' ]* <alphadigit> ]
    }
    token alphadigit  { <alpha> | <digit> }
    token hostnumber  { <digits> '.' <digits> '.' <digits> '.' <digits> }
    token port        { <digits> }
    token user        { [ <uchar> | ';' | '?' | '&' | '=' ]* }
    token password    { [ <uchar> | ';' | '?' | '&' | '=' ]* }
    token urlpath     { <xchar>* }
    
    # FTP
    token ftpurl   { 'ftp://' <login> [ '/' <fpath> [ ';type=' <ftptype> ]? ]? }
    token fpath    { <fsegment> [ '/' <fsegment> ]* }
    token fsegment { [ <uchar> | '?' | ':' | '@' | '&' | '=' ]* }
    token ftyptype { 'A' | 'I' | 'D' | 'a' | 'i' | 'd' }

    # FILE
    token fileurl { 'file://' [ <host> | 'localhost' ]? '/' <fpath> }

    # HTTP
    token httpurl  { 'http://' <hostport> [ '/' <hpath> [ '?' <search> ]? ]? }
    token hpath    { <hsegment> [ '/' <hsegment> ]* }
    token hsegment { [ <uchar> | ';' | ':' | '@' | '&' | '=' ]* }
    token search   { [ <uchar> | ';' | ':' | '@' | '&' | '=' ]* }

    # GOPHER
    token gopherurl      {
        'gopher://'
        <hostport>
        [ '/'
            [ <gtype>
                [ <selector>
                    [ '%09' <search>
                        [ '%09' <gopher__string> ]?
                    ]?
                ]?
            ]?
        ]?
    }
    token gtype          { <xchar> }
    token selector       { <xchar>* }
    token gopher__string { <xchar>* }
    
    # MAILTO
    token mailtourl      { 'mailto:' <encoded822addr> }
    token encoded822addr { <xchar>**{ 1 .. * } }
        # "further defined in RFC822".  yeah, okay.
    
    # NEWS
    token newsurl   { 'news:' <grouppart> }
    token grouppart { '*' | <group> | <article> }
    token group     { <alpha> [ <alpha> | <digit> | '-' | '.' | '+' | '_' ]* }
    token article   {
        [ <uchar> | ';' | '/' | '?' | ':' | '&' | '=' ]**{ 1 .. * } '@' <host>
    }
    
    # NNTP
    token nntpurl { 'nttp://' <hostport> '/' <group> [ '/' <digits> ]? }
    
    # TELNET
    token telneturl { 'telnet://' <login> '/'? }
    
    # WAIS
    token waisurl      { <waisdatabase> | <waisindex> | <waisdoc> }
    token waisdatabase { 'wais://' <hostport> '/' <database> }
    token waisindex    { 'wais://' <hostport> '/' <database> '?' <search> }
    token waisdoc      {
        'wais://' <hostport> '/' <database> '/' <wtype> '/' <wpath>
    }
    token database     { <uchar>* }
    token wtype        { <uchar>* }
    token wpath        { <uchar>* }

    # PROSPERO
    token prosperourl { 'prospero://' <hostport> '/' <ppath> <fieldspec>* }
    token ppath       { <psegment> [ '/' <psegment> ]* }
    token psegment    { [ <uchar> | < ? : @ & = > ]* }
    token fieldspec   { ';' <fieldname> '=' <fieldvalue> }
    token fieldname   { [ <uchar> | < ? : @ & > ]* }
    token fieldvalue  { [ <uchar> | < ? : @ & > ]* }

    # miscellaneous definitions
    token lowalpha { <[ \x61 .. \x7A ]> } # a-z
    token hialpha  { <[ \x41 .. \x5A ]> } # A-Z
    
    token alpha       { <lowalpha> | <hialpha> }
    token digit       { <[ \x30 .. \x39 ]> } # 0–9
    token safe        { < $ - _ . + > }
    token extra       { < ! * ' ( ) , > }
    token national    { < { } | \\ ^ ~ [ ] ` > }
    token punctuation { <[ \< \> \# \% \" ]> }
    
    token reserved { < ; / ? : @ & = > }
    token hex      { <digit> | <[ A B C D E F a b c d e f ]> }
    token escape   { '%' <hex> <hex> }
    
    token unreserved { <alpha> | <digit> | <safe> | <extra> }
    token uchar      { <unreserved> | <escape> }
    token xchar      { <unreserved> | <reserved> | <escape> }
    token digits     { <digit>**{ 1 .. * } }
}