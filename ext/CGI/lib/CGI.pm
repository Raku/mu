use v6-alpha;
class CGI-0.2;
    # XXX Should this all be rw? Should any be public?
    has %!PARAMS           is rw;
    has $!REQUEST_METHOD   is rw;
    has $!CONTENT_LENGTH   is rw;
    has $!CONTENT_TYPE     is rw;
    has $!QUERY_STRING     is rw;
    # I would prefer this syntax, but it seems not be supported yet. 
    # has $!QS_DELIMITER     is rw = ';';
    has $!QS_DELIMITER     is rw; 
    has $!URL_ENCODING     is rw; 
    has $!IS_PARAMS_LOADED is rw; 

# Use method, not submethod, because we do what these behaviors to be inherited.  
method BUILD (*%param) {
        $!QS_DELIMITER     = ';';
        $!URL_ENCODING     = 'iso-8859-1';
        $!IS_PARAMS_LOADED = 0;
        %!PARAMS = %param if %param;
}

## methods

# information methods

method clear_params returns Void { %!PARAMS = () }
method reset_params returns Void { %!PARAMS = (); $!IS_PARAMS_LOADED = 0; }

method query_string   returns Str { $!QUERY_STRING   }
method request_method returns Str { $!REQUEST_METHOD }
method content_type   returns Str { $!CONTENT_TYPE   }
method content_length returns Str { $!CONTENT_LENGTH }

# make some of the less used values 'on demand'

method path_info       returns Str { %*ENV<PATH_INFO> || '' }
method request_uri     returns Str { %*ENV<REQUEST_URI>   }
method referer         returns Str { %*ENV<HTTP_REFERER>  }
method document_root   returns Str { %*ENV<DOCUMENT_ROOT> }
method script_name     returns Str { 
    %*ENV<SCRIPT_NAME> || $*PROGRAM_NAME
}

# do we have a way to set "optional" exporting?
method set_delimiter(Str $delimiter) {
    unless $delimiter eq (';' | '&') {
        die "Query string delimiter must be a semi-colon or ampersand";
    }
    $!QS_DELIMITER = $delimiter;
}

# set GET and POST parameters encoding
method set_url_encoding(Str $encoding) {
    unless $encoding eq ('iso-8859-1' | 'utf-8') {
    die "Currently iso-8859-1 and utf-8 encodings supported";
    }
    $!URL_ENCODING = $encoding;
}

# utility functions

method header (
    Str  $content_type? = 'text/html',
    Str  $status?       = '200 OK',
    Str  $charset?      = undef,
    Str :$cookies?,
    Str :$target?,
    :$expires?,
    Bool :$nph?,
    *%extra
) returns Str {
    # construct our header
    my $header;
    $header ~= "Status: " ~ $status;
    # TODO:
    # Need to add support for -
    #    NPH
    #    Expires:
    #    Pragma: (caching)
    
    $header ~= "\nContent-Type: " ~ $content_type;
    $header ~= "; charset=$charset" if $charset.defined;
    
    for %extra.kv -> $key, $value {
        # XXX use $key is rw;
        my $temp_key = ucfirst(lc($key));
        
        $temp_key ~~ s:P5:g/[-_](\w)/-$0.uc()/;
        
        given $key {
            when "Target" { $header ~= "\nWindow-Target: "     ~ $value; }
            default       { $header ~= "\n" ~ $temp_key ~ ": " ~ $value; }
        }
    }
    
    if ($cookies) {
        my @cookies = ($cookies !~~ Array) ?? ($cookies) !! @$cookies;
        
        for @cookies -> $cookie {
            #$cookie = ($cookie ~~ CGI::Cookie) ?? $cookie.as_string !! $cookie;
            
            $header ~= "\nSet-Cookie: " ~ $cookie
                unless $cookie eq "";
        }
    }
    
    return "$header\n\n";
}

method redirect (
    Str   $location,
    Str   $target?,
    Str   $status? = "302 Found",
    Str  :$cookie,
    Bool :$nph,
    *%extra
) returns Str {
    my %out;
    
    # XXX provide default for $location
    #$location //= $self.location;
    
    # XXX just clone %extra
    #%out = %extra.clone();
    
    %out<Location> = $location;
    
    for %extra.kv -> $header, $value {
        %out{$header} = $value;
    }
    
    if ($target.defined) { %out<Target> = $target; }
    
    for %out.keys -> $key {
        %out{$key} = self.unescapeHTML(%out{$key})
            unless $key eq "Cookie";
    }
    
    if ($cookie.defined) {
        return self.header('', $status, cookies => $cookie, nph => $nph, extra => %out);
    } else {
        return self.header('', $status, nph => $nph, extra => %out);
    }
}

method url_decode (Str $to_decode) returns Str {
    my $decoded = $to_decode;
    $decoded ~~ s:P5:g/\+/ /;
    given $!URL_ENCODING {
        when 'iso-8859-1' {
            $decoded ~~ s:P5:g/%([\da-fA-F][\da-fA-F])/{chr(:16($0))}/;
        }
        when 'utf-8' {
            $decoded ~~ s:P5:g:i/%(F[CD])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&1)*1073741824+(:16($1)+&63)*16777216+(:16($2)+&63)*262144+(:16($3)+&63)*4096+(:16($4)+&63)*64+(:16($5)+&63))}/;
            $decoded ~~ s:P5:g:i/%(F[8-B])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&3)*16777216+(:16($1)+&63)*262144+(:16($2)+&63)*4096+(:16($3)+&63)*64+(:16($4)+&63))}/;
            $decoded ~~ s:P5:g:i/%(F[0-7])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&7)*262144+(:16($1)+&63)*4096+(:16($2)+&63)*64+(:16($3)+&63))}/;
            $decoded ~~ s:P5:g:i/%(E[\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&15)*4096+(:16($1)+&63)*64+(:16($2)+&63))}/;
            $decoded ~~ s:P5:g:i/%([CD][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&31)*64+(:16($1)+&63))}/;
            $decoded ~~ s:P5:g:i/%([0-7][\dA-F])/{chr(:16($0))}/;
        }
    }
    return $decoded;
}

method url_encode (Str $to_encode) returns Str {
    my $encoded = $to_encode;
    # create a simplistic dec-to-hex converter
    # which will be able to handle the 0-255 values
    my @hex = <0 1 2 3 4 5 6 7 8 9 A B C D E F>;
    my $dec2hex = -> $dec { '%' ~ @hex[int($dec / 16)+&15] ~ @hex[$dec % 16]; };
    # create 
    my $utf82hex = -> $num {
        if ($num < 128) { $dec2hex($num); }
        elsif ($num < 2048) { $dec2hex(192+$num/64)~$dec2hex(128+$num%64); }
        elsif ($num < 65536) { $dec2hex(224+$num/4096)~$dec2hex(128+($num/64)%64)~$dec2hex(128+$num%64); }
        elsif ($num < 2097152) { $dec2hex(240+$num/262144)~$dec2hex(128+($num/4096)%64)~$dec2hex(128+($num/64)%64)~$dec2hex(128+$num%64); }
        elsif ($num < 67108864) { $dec2hex(248+$num/16777216)~$dec2hex(128+($num/262144)%64)~$dec2hex(128+($num/4096)%64)~$dec2hex(128+($num/64)%64)~$dec2hex(128+$num%64); }
        else { $dec2hex(252+$num/1073741824)~$dec2hex(248+($num/16777216)%64)~$dec2hex(128+($num/262144)%64)~$dec2hex(128+($num/4096)%64)~$dec2hex(128+($num/64)%64)~$dec2hex(128+$num%64); }
    };
    given $!URL_ENCODING {
        when 'iso-8859-1' {
            $encoded ~~ s:P5:g/([^-.\w])/$dec2hex(ord($0))/;
        }
        when 'utf-8' {
            $encoded ~~ s:P5:g/([^-.\w])/$utf82hex(ord($0))/;
        }
    }
    return $encoded;
}

method pack_params returns Str {
    my @packed_params;
    for (%!PARAMS.kv) -> $param, $value {
        for $value -> $val {
            @packed_params.push(self.url_encode($param) ~ '=' ~ self.url_encode($val));                            
        }
    }
    return join($!QS_DELIMITER, @packed_params);
}

method unpack_params (Str $data) returns Str {
    my @pairs = split(rx:P5/[&;]/, $data);
    for @pairs -> $pair {
        my ($key, $value) = split('=', $pair);
        $key = self.url_decode($key);
        if (%!PARAMS{"$key"}) {
            my $list := %!PARAMS{"$key"};
            $list.push(self.url_decode($value));
        }
        else {
            %!PARAMS{"$key"} = [ self.url_decode($value) ];            
        }
    }  
}

method load_params {
    $!IS_PARAMS_LOADED = 1; 
    ## initialize all the globals
    try {
        $!REQUEST_METHOD = %*ENV<REQUEST_METHOD>;
        $!CONTENT_TYPE   = %*ENV<CONTENT_TYPE>;    
        $!CONTENT_LENGTH = %*ENV<CONTENT_LENGTH>;   
            
        if (lc($!REQUEST_METHOD) eq ('get' | 'head')) {
            $!QUERY_STRING = %*ENV<QUERY_STRING>;
            unpack_params($!QUERY_STRING) if $!QUERY_STRING;
        }
        elsif (lc($!REQUEST_METHOD) eq 'post') { 
            if (!$!CONTENT_TYPE || $!CONTENT_TYPE eq 'application/x-www-form-urlencoded') {
                my $content; # = read($*IN, $!CONTENT_LENGTH);
                unpack_params($content) if $content;
            }
        }
        elsif (@*ARGS) {
            my $input = join('', @*ARGS);
            unpack_params($input);
        }
        else {
            die "Invalid Content Type" if $!REQUEST_METHOD; # only die if we are running under CGI
        }
    };
    if ($!) {
        print header;
        say "There was an error getting the params:\n\t" ~ $!;
        exit();
    }    
}

method escapeHTML (Str $string, Bool :$newlines) returns Str {
    # XXX check for $self.escape == 0
    #unless ($self.escape != 0) { return $toencode; }
    
    $string ~~ s:P5:g/&/&amp;/;
    $string ~~ s:P5:g/</&lt;/;
    $string ~~ s:P5:g/>/&gt;/;
    
    # XXX check for HTML 3.2
    #if ($self.DTD_PUBLIC_IDENTIFIER ~~ rx:P5/[^X]HTML 3\.2/i) {
        # $quot; was accidentally omitted from the HTML 3.2 DTD -- see
        # <http://validator.w3.org/docs/errors.html#bad-entity> /
        # <http://lists.w3.org/Archives/Public/www-html/1997Mar/0003.html>.
        
        #$string ~~ s:P5:g/"/&#34;/;
    #} else {
        $string ~~ s:P5:g/"/&quot;/;
    #}
    
    my $latin;
    
    # XXX check $self.charset
    #$latin = ?(uc $self.charset eq "ISO-8859-1"|"WINDOWS-1252");
    $latin = 1;
    
    if ($latin) {
        $string ~~ s:P5:g/'/&#39;/;
        $string ~~ s:P5:g/\x8b/&#8249;/;
        $string ~~ s:P5:g/\x9b/&#8250;/;
        
        if ($newlines) {
            $string ~~ s:P5:g/\012/&#10;/;
            $string ~~ s:P5:g/\015/&#13;/;
        }
    }
    
    return $string;
}

method unescapeHTML (Str $string) returns Str {
    # XXX check $self.charset
    #my $latin = ?(uc $self.charset eq "ISO-8859-1"|"WINDOWS-1252");
    my $latin = 1;
    
    $string ~~ s:P5:g/&([^;]*);/{
        given (lc $1) {
            when "amp"  { "&" }
            when "quot" { '"' }
            when "gt"   { ">" }
            when "lt"   { "<" }
            
            if ($latin) {
                when /^#(\d+)$/     { chr($1) }
                when /^#x(\d+)$/    { chr(:16($1)) }
            }
        }
    }/;
    
    return $string;
}

# information functions (again)

multi method param returns Array            { unless $!IS_PARAMS_LOADED {self.load_params}; %!PARAMS.keys;    }
multi method param (Str $key) returns Array { unless $!IS_PARAMS_LOADED {self.load_params}; (%!PARAMS{$key}); }

=pod

=head1 NAME

CGI - A module for programming CGI

=head1 SYNOPSIS

    use v6-alpha;
    use CGI;

    my $q = CGI.new;
    
    print $q.header;
    
    if ($q.param) {
        for $q.param -> $key {
            say $key ~ " => " ~ $q.param($key) ~ "<BR>";
        }
    }
    else {
        say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
    }
    
    # you can also test it on the command line too
    % pugs -I lib/ examples/test.pl "greetings=hello world"    

=head1 DESCRIPTION

CGI for Perl6!

=head1 METHODS

=head2 Constructor

=head3 new()

Create a new object CGI object. 

 my $q = CGI.new;

You can also initialize the object with your own hash of parameters:

 my $q = CGI.new( a => 'b');

=head2 Informational

=over 4

=item B<param returns Array>

=item B<param (Str $key) returns Array>

=item B<query_string returns Str>

=item B<request_method returns Str>

=item B<content_type returns Str>

=item B<content_length returns Str>

=back

B<The following informational functions are fetched on-demand>

=over 4

=item B<path_info returns Str>

=item B<referer returns Str>

=item B<request_uri returns Str>

=item B<document_root returns Str>

=item B<script_name returns Str>

=back

=head2 Utility

=over 4

=item B<header (:$status = '200 OK', :$content_type = 'text/html', :$charset, :$location) returns Str>

=item B<redirect (Str $location) returns Str>

=item B<url_decode (Str $to_decode) returns Str>

=item B<url_encode (Str $to_encode) returns Str>

=item B<pack_params returns Str>

=item B<unpack_params (Str $data) returns Str>

=back

=head1 TO DO

=over 4

=item I<Cookies>

=back

=head1 AUTHORS

stevan little, E<lt>stevan@iinteractive.comE<gt>

Audrey Tang, E<lt>autrijus@autrijus.comE<gt>

Curtis "Ovid" Poe
 
Andras Barthazi, E<lt>andras@barthazi.huE<gt>

"Aankhen"

Mark Stosberg

=head1 COPYRIGHT

Parts Copyright (c) 2005. Stevan Little. All rights reserved.
Parts Copyright (c) 2006. Mark Stosberg. 

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
# vim: ft=perl6
