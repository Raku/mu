
my $SID;
my %Session;

sub SessionInit() {
    my %Session=();
    $SID=GetCookie('sid'); $SID ~~ s:perl5:g/[^A-Z]//;
    if (!-e "sids/$SID") { $SID=''; }
    $SID=IDGenerate() if $SID eq '';
    SessionDecode(slurp("sids/$SID"));
    CookiesAdd('sid',$SID);
}

sub SessionID() { return $SID; }

sub SessionSet($variable, $value) {
    %Session{$variable} = $value;
}

sub SessionGet($variable) {
    return %Session{$variable};
}

sub SessionClear() {
    %Session=();
}

sub SessionCode(@variables) {
    my(@coded);
    for(@variables) {
        push(@coded,$_ ~ "=" ~ url_encode(%Session{$_}));
    }
    return @coded.join('&');
}
 
sub SessionDecode($data) {
    for(split('&',$data)) {
        my($variable,$value) = split('=',$_);
        %Session{$variable} = url_decode($value) if ($variable ne '');
    } 
}

sub SessionDestroy() {
    my $fh = open(">sids/$SID");
    $fh.print(SessionCode(%Session.keys));
    $fh.close;
}

SessionInit();
