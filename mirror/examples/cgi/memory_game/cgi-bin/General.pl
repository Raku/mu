
# -----------------------------------------------
#  Copyright (C) 2005 by Andras Barthazi
#  This code is totally free, use it as you want
# -----------------------------------------------
#  !!WARNING!!  UGLY, NON-COMMENTED  !!WARNING!!
# -----------------------------------------------

sub IDGenerate {
    my($id)='';
    for (1..32) { $id~=chr(65+int(rand 26)); }
    return($id);
}

sub PageHeader (
    +$status = '200 OK', 
    +$content_type = 'text/html', 
    +$charset, 
    +$location
) returns Str {
    # construct our header
    my $header;
    $header ~= "Status: " ~ $status ~ "\n";
    # TODO:
    # Need to add support for -
    #    Expires:
    #    Pragma: (caching)
    #    Set-Cookie: (multiple cookies)
    print CookiesHeader();
    if ($location.defined) {
        $header ~= "Location: " ~ $location;
    }
    else {
        $header ~= "Content-type: " ~ $content_type;
        $header ~= "; charset=$charset" if $charset.defined;        
    }
    return "$header\n\n";
}
