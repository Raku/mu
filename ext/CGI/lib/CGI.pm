module CGI-0.0.1;
use v6;

## set up all the globals (which will eventually be object attributes)

my %PARAMS;

my $REQUEST_METHOD;
my $CONTENT_LENGTH;
my $CONTENT_TYPE;
my $QUERY_STRING;
my $QS_DELIMITER = ';';
my $URL_ENCODING = 'iso-8859-1';
my $IS_PARAMS_LOADED = 0;

## functions

# information functions

sub clear_params returns Void is export { %PARAMS = () }
sub reset_params returns Void is export { %PARAMS = (); $IS_PARAMS_LOADED = 0; }

sub query_string   returns Str is export { $QUERY_STRING   }
sub request_method returns Str is export { $REQUEST_METHOD }
sub content_type   returns Str is export { $CONTENT_TYPE   }
sub content_length returns Str is export { $CONTENT_LENGTH }

# make some of the less used values 'on demand'

sub path_info       returns Str is export { %*ENV<PATH_INFO> || '' }
sub request_uri     returns Str is export { %*ENV<REQUEST_URI>   }
sub referer         returns Str is export { %*ENV<HTTP_REFERER>  }
sub document_root   returns Str is export { %*ENV<DOCUMENT_ROOT> }
sub script_name     returns Str is export { 
    %*ENV<SCRIPT_NAME> || $*PROGRAM_NAME
}

# do we have a way to set "optional" exporting?
sub set_delimiter(Str $delimiter) is export {
    unless $delimiter eq (';' | '&') {
        die "Query string delimiter must be a semi-colon or ampersand";
    }
    $QS_DELIMITER = $delimiter;
}

# set GET and POST parameters encoding
sub set_url_encoding(Str $encoding) is export {
    unless $encoding eq ('iso-8859-1' | 'utf-8') {
    die "Currently iso-8859-1 and utf-8 encodings supported";
    }
    $URL_ENCODING = $encoding;
}

# utility functions

sub header (
    Str  $content_type? = 'text/html',
    Str  $status? = '200 OK',
    Str  $charset? = undef,
    Str :$cookies?,
    Str :$target?,
    :$expires?,
    Bool :$nph?,
    *%extra
) returns Str is export {
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
            when "Target" { $header ~= "\nWindow-Target: " ~ $value; }
            default { $header ~= "\n" ~ $temp_key ~ ": " ~ $value; }
        }
    }
    
    if ($cookies) {
        my @cookies = ($cookies !~ Array) ?? ($cookies) !! @{$cookies};
        
        for @cookies -> $cookie {
            #$cookie = ($cookie ~~ CGI::Cookie) ?? $cookie.as_string !! $cookie;
            
            $header ~= "\nSet-Cookie: " ~ $cookie
                unless $cookie eq "";
        }
    }
    
    return "$header\n\n";
}

sub redirect (
    Str   $location,
    Str   $target?,
    Str   $status? = "302 Found",
    Str  :$cookie,
    Bool :$nph,
    *%extra
) returns Str is export {
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
        %out{$key} = unescapeHTML(%out{$key})
            unless $key eq "Cookie";
    }
    
    if ($cookie.defined) {
        return header('', $status, cookies => $cookie, nph => $nph, extra => %out);
    } else {
        return header('', $status, nph => $nph, extra => %out);
    }
}

sub url_decode (Str $to_decode) returns Str is export {
    my $decoded = $to_decode;
    $decoded ~~ s:perl5:g/\+/ /;
    given $URL_ENCODING {
        when 'iso-8859-1' {
            $decoded ~~ s:perl5:g/%([\da-fA-F][\da-fA-F])/{chr(:16($0))}/;
        }
        when 'utf-8' {
            $decoded ~~ s:perl5:g:i/%(F[CD])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&1)*1073741824+(:16($1)+&63)*16777216+(:16($2)+&63)*262144+(:16($3)+&63)*4096+(:16($4)+&63)*64+(:16($5)+&63))}/;
            $decoded ~~ s:perl5:g:i/%(F[8-B])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&3)*16777216+(:16($1)+&63)*262144+(:16($2)+&63)*4096+(:16($3)+&63)*64+(:16($4)+&63))}/;
            $decoded ~~ s:perl5:g:i/%(F[0-7])%([8-9AB][\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&7)*262144+(:16($1)+&63)*4096+(:16($2)+&63)*64+(:16($3)+&63))}/;
            $decoded ~~ s:perl5:g:i/%(E[\dA-F])%([8-9AB][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&15)*4096+(:16($1)+&63)*64+(:16($2)+&63))}/;
            $decoded ~~ s:perl5:g:i/%([CD][\dA-F])%([8-9AB][\dA-F])/{chr((:16($0)+&31)*64+(:16($1)+&63))}/;
            $decoded ~~ s:perl5:g:i/%([0-7][\dA-F])/{chr(:16($0))}/;
        }
    }
    return $decoded;
}

sub url_encode (Str $to_encode) returns Str is export {
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
    given $URL_ENCODING {
        when 'iso-8859-1' {
            $encoded ~~ s:perl5:g/([^-.\w])/$dec2hex(ord($0))/;
        }
        when 'utf-8' {
            $encoded ~~ s:perl5:g/([^-.\w])/$utf82hex(ord($0))/;
        }
    }
    return $encoded;
}

sub pack_params returns Str is export {
    my @packed_params;
    for (%PARAMS.kv) -> $param, $value {
        for $value -> $val {
            @packed_params.push(url_encode($param) ~ '=' ~ url_encode($val));                            
        }
    }
    return join($QS_DELIMITER, @packed_params);
}

sub unpack_params (Str $data) returns Str is export {
    my @pairs = split(rx:perl5{[&;]}, $data);
    for @pairs -> $pair {
        my ($key, $value) = split('=', $pair);
        $key = url_decode($key);
        if (%PARAMS{"$key"}) {
            my $list := %PARAMS{"$key"};
            $list.push(url_decode($value));
        }
        else {
            %PARAMS{"$key"} = [ url_decode($value) ];            
        }
    }  
}

sub load_params {
    $IS_PARAMS_LOADED = 1; 
    ## initialize all the globals
    try {
        $REQUEST_METHOD = %*ENV<REQUEST_METHOD>;
        $CONTENT_TYPE   = %*ENV<CONTENT_TYPE>;    
        $CONTENT_LENGTH = %*ENV<CONTENT_LENGTH>;   
            
        if (lc($REQUEST_METHOD) eq ('get' | 'head')) {
            $QUERY_STRING = %*ENV<QUERY_STRING>;
            unpack_params($QUERY_STRING) if $QUERY_STRING;
        }
        elsif (lc($REQUEST_METHOD) eq 'post') { 
            if (!$CONTENT_TYPE || $CONTENT_TYPE eq 'application/x-www-form-urlencoded') {
                my $content; # = read($*IN, $CONTENT_LENGTH);
                unpack_params($content) if $content;
            }
        }
        elsif (@ARGS) {
            my $input = join('', @ARGS);
            unpack_params($input);
        }
        else {
            die "Invalid Content Type" if $REQUEST_METHOD; # only die if we are running under CGI
        }
    };
    if ($!) {
        print header;
        say "There was an error getting the params:\n\t" ~ $!;
        exit();
    }    
}

sub escapeHTML (Str $string, Bool :$newlines) returns Str is export {
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

sub unescapeHTML (Str $string) returns Str is export {
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

multi sub param returns Array is export { unless($IS_PARAMS_LOADED) {load_params}; %PARAMS.keys; }
multi sub param (Str $key) returns Array is export { unless($IS_PARAMS_LOADED) {load_params}; (%PARAMS{$key}); }

=pod

=head1 NAME

CGI - A module for programming CGI

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;
    require CGI-0.0.1;
    
    print header;
    
    if (param()) {
        for param() -> $key {
            say $key ~ " => " ~ param($key) ~ "<BR>";
        }
    }
    else {
        say "<FORM><INPUT TYPE='text' NAME='test'><INPUT TYPE='submit'></FORM>";
    }
    
    # you can also test it on the command line too
    % pugs -I lib/ examples/test.p6 "greetings=hello world"    

=head1 DESCRIPTION

CGI for Pugs! 

=head1 FUNCTIONS

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

My inclination is to wait until objects are done for this, but maybe I will
figure out a good way to do this without. Either way it is still TODO.

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

Audrey Tang, E<lt>autrijus@autrijus.comE<gt>

Curtis "Ovid" Poe
 
Andras Barthazi, E<lt>andras@barthazi.huE<gt>

"Aankhen"

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
