
# -----------------------------------------------
#  Copyright (C) 2005 by Andras Barthazi
#  This code is totally free, use it as you want
# -----------------------------------------------
#  !!WARNING!!  UGLY, NON-COMMENTED  !!WARNING!!
# -----------------------------------------------

my %Cookies;

sub CookiesInit() {
    %Cookies=();
    if (%*ENV{'HTTP_COOKIE'} ne '') {
        for (split(rx:perl5/\s*;\s*/,%*ENV{'HTTP_COOKIE'})) {
            my ($p,$v)=split('=',$_);
            CookiesAdd($p,$v);
        }
    }
}

sub CookiesHeader() {
    my $header = '';
    for(%Cookies.keys) {
        $header ~= "Set-Cookie: " ~ $_ ~ "=" ~ %Cookies{$_} ~ "; expires=Sat, 01-Jan-3000 12:00:00 GMT\n";
    }
    return($header);
}
 
sub CookiesAdd($variable, $value) {
    %Cookies{$variable}=$value;
}

sub GetCookie($variable) {
    return %Cookies{$variable};
}

CookiesInit();
