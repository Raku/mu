use v6;
class CGI:ver<0.3>;
    # XXX Should this all be rw? Should any be public?
    has %.params;
    has $.request_method;
    has $.content_length;
    has $.content_type;
    has $.query_string;
    # I would prefer this syntax, but it seems not be supported yet. 
    # has $!QS_DELIMITER = ';';
    has $!QS_DELIMITER = ';';
    has $!URL_ENCODING = 'ISO-8859-1';
    has $!IS_PARAMS_LOADED = 0;
    has $!CHARSET      = 'ISO-8859-1';

# Use method, not submethod, because we do what these behaviors to be inherited.  
method BUILD (*%param) {
        # set charset to the safe ISO-8859-1
        %!params = %param if %param;
}

## methods

# information methods

method clear_params { %!params = () }
method reset_params { %!params = (); $!IS_PARAMS_LOADED = 0; }

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
    Str        $type      = 'text/html',
    Str        $charset?,
              :$cookie?,
    Str       :$target?,
              :$expires?,
    Bool      :$nph?,
    *%extra
) {
    # construct our header
    my $header;
    # TODO:
    # Need to add support for -
    #    NPH
    #    Expires:
    #    Pragma: (caching)

    
    if $type {
        $header ~= "Content-Type: " ~ $type;
        $header ~= "; charset=$charset" if $charset.defined;
        $header ~= "\n";
    }
    
    for %extra.keys.sort -> $key is copy {
        # XXX use $key is rw;
        my $value = %extra{$key};
        my $temp_key = ucfirst(lc($key));
        
        $temp_key ~~ s:g[ <-[ _ ]> <alnum> ] = -$<alnum>.uc;
        
        given $key {
            when "Target" { $header ~= "Window-Target: "     ~ $value~"\n"; }
            default       { $header ~= "" ~ $temp_key ~ ": " ~ $value~"\n"; }
        }
    }

    if ($cookie) {
        for $cookie.flat -> $one {
            #$cookie = ($cookie ~~ CGI::Cookie) ?? $cookie.as_string !! $cookie;
            
            $header ~= "Set-Cookie: " ~ $one~"\n" if $one.chars;
        }
    }

    return "$header\n";
}

method redirect (
    Str   $location,
    Str   $target?,
    Str  :$status = "302 Found",
    Str  :$cookie,
    Bool :$nph,
    *%extra
) {
    my %out;

    # XXX provide default for $location
    #$location //= $self.location;
    
    # XXX just clone %extra
    #%out = %extra.clone();
    
    %out<Location> = $location;
    
    for %extra.kv -> $header, $value {
        %out{$header} = $value;
    }
    
    if $target.defined { %out<Target> = $target; }
    
    for %out.keys -> $key {
        %out{$key} = self.unescapeHTML(%out{$key})
            unless $key eq "Cookie";
    }

    my $header =  "Status: $status\n";
    
    if $cookie.defined {
        return $header~self.header('', cookie => $cookie, nph => $nph, extra => %out);
    } else {
        return $header~self.header('', nph => $nph, extra => %out);
    }
}

method url_decode (Str $to_decode) returns Str {
    my $decoded = $to_decode;
    $decoded ~~ s:g/\+/ /;
    given $!URL_ENCODING {
        when 'iso-8859-1' {
            $decoded ~~ s:g/(<xdigit>**2)/{chr(:16($0))}/;
        }
        when 'utf-8' {
            # TODO: proper UTF-8 decoding
#            $decoded ~~ s:g:i/(F[CD])%([8-9AB][<digit>A..F])%([8-9AB][<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])/{chr((:16($0)+&1)*1073741824+(:16($1)+&63)*16777216+(:16($2)+&63)*262144+(:16($3)+&63)*4096+(:16($4)+&63)*64+(:16($5)+&63))}/;
#            $decoded ~~ s:g:i/%(F[8..B])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])/{chr((:16($0)+&3)*16777216+(:16($1)+&63)*262144+(:16($2)+&63)*4096+(:16($3)+&63)*64+(:16($4)+&63))}/;
#            $decoded ~~ s:g:i/%(F[0..7])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])/{chr((:16($0)+&7)*262144+(:16($1)+&63)*4096+(:16($2)+&63)*64+(:16($3)+&63))}/;
#            $decoded ~~ s:g:i/%(E[<digit>A..F])%([8..9AB][<digit>A..F])%([8..9AB][<digit>A..F])/{chr((:16($0)+&15)*4096+(:16($1)+&63)*64+(:16($2)+&63))}/;
#            $decoded ~~ s:g:i/%([CD][<digit>A..F])%([8..9AB][<digit>A..F])/{chr((:16($0)+&31)*64+(:16($1)+&63))}/;
#            $decoded ~~ s:g:i/%([0..7][<digit>A..F])/{chr(:16($0))}/;
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
            $encoded ~~ s:g/(<-[\-.]-alnum>)/{$dec2hex(ord($0))}/;
        }
        when 'utf-8' {
            $encoded ~~ s:g/(<-[\-.]-alnum>)/{$utf82hex(ord($0))}/;
        }
    }
    return $encoded;
}

method pack_params returns Str {
    join $!QS_DELIMITER, gather {
        for %!params.keys.sort -> $param {
            for each(%!params{$param}) -> $val {
                take(self.url_encode($param) ~ '=' ~ self.url_encode($val));                            
            }
        }
    };
}

method unpack_params (Str $data) returns Str {
    my @pairs = split(rx/<[&;]>/, $data);
    for @pairs -> $pair {
        my ($key, $value) = split('=', $pair);
        $key = self.url_decode($key);
        %!params{$key} //= [];
        %!params{$key}.push( self.url_decode($value) );
    }  
}

method load_params {
    $!IS_PARAMS_LOADED = 1; 
    ## initialize all the globals
    try {
        $!request_method = %*ENV<REQUEST_METHOD>;
        $!content_type   = %*ENV<CONTENT_TYPE>;    
        $!content_length = %*ENV<CONTENT_LENGTH>;   
            
        if (lc($!request_method) eq ('get' | 'head')) {
            $!query_string = %*ENV<QUERY_STRING>;
            self.unpack_params($!query_string) if $!query_string;
        }
        elsif (lc($!request_method) eq 'post') { 
            if (!$!content_type || $!content_type eq 'application/x-www-form-urlencoded') {
                my $content; # = read($*IN, $!content_length);
                self.unpack_params($content) if $content;
            }
        }
        elsif (@*ARGS) {
            my $input = join('', @*ARGS);
            self.unpack_params($input);
        }
        else {
            die "Invalid Content Type" if $!request_method; # only die if we are running under CGI
        }
    };
    if ($!) {
        print self.header;
        say "There was an error getting the params:\n\t" ~ $!;
        exit();
    }    
}

method escapeHTML (Str $string is copy, Bool :$newlines) returns Str {
    # XXX check for $self.escape == 0
    #unless ($self.escape != 0) { return $toencode; }
    
    $string ~~ s:g/\&/&amp;/;
    $string ~~ s:g/\</&lt;/;
    $string ~~ s:g/\>/&gt;/;
    
    # XXX check for HTML 3.2
    #if ($self.DTD_PUBLIC_IDENTIFIER ~~ rx:i/<-[X]>HTML 3\.2/) {
        # $quot; was accidentally omitted from the HTML 3.2 DTD -- see
        # <http://validator.w3.org/docs/errors.html#bad-entity> /
        # <http://lists.w3.org/Archives/Public/www-html/1997Mar/0003.html>.
        
        #$string ~~ s:g/"/&#34;/;
    #} else {
        $string ~~ s:g/\"/&quot;/;
    #}
    
    my $latin;
    
    # XXX check $self.charset
    #$latin = ?(uc $self.charset eq "ISO-8859-1"|"WINDOWS-1252");
    $latin = 1;
    
    if ($latin) {
        $string ~~ s:g/\'/&#39;/;
        $string ~~ s:g/\x8b/&#8249;/;
        $string ~~ s:g/\x9b/&#8250;/;
        
        if ($newlines) {
            $string ~~ s:g/\o12/&#10;/;
            $string ~~ s:g/\o15/&#13;/;
        }
    }
    
    return $string;
}

method unescapeHTML (Str $string is copy) returns Str {

    my $latin = ?(uc $!CHARSET ~~ "ISO-8859-1"|"WINDOWS-1252");

    $string ~~ s:g[(<-[ ; ]>*)\;] =
        do given lc($0) {
            when "amp"  { "&" }
            when "quot" { '"' }
            when "gt"   { ">" }
            when "lt"   { "<" }
            when m{^\#(<digit>+)$}     && $latin { chr($1) }
            when m:i{^\#x(<xdigit>+)$} && $latin { chr(hex($1)) }
            default { $0  }
        };

    return $string;
}

# information functions (again)

multi method param            { unless $!IS_PARAMS_LOADED {self.load_params}; %!params.keys.sort;  }
multi method param (Str $key) { unless $!IS_PARAMS_LOADED {self.load_params}; %!params{$key}; }

method Dump {
    return '<ul></ul>' unless self.param;

    join "\n", gather {
        take "<ul>";

        for self.param -> $param {
            my $name = self.escapeHTML($param);
            take("<li><strong>$name </strong></li>");
            take(" <ul>");
            for each(self.param($param)) -> $value {
                my $esc_val = self.escapeHTML($value);
                $esc_val ~~ s:g/\n/<br \/>\n/;
                take("<li>$esc_val </li>");
            }
            take("</ul>");
        }

        take "</ul>";
    }
}

=begin pod

=head1 NAME

CGI - A module for programming CGI

=head1 SYNOPSIS

    use v6;
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

=item B<header (:$content_type = 'text/html', :$charset, :$location) returns Str>

=item B<redirect (Str $location) returns Str>

=item B<url_decode (Str $to_decode) returns Str>

=item B<url_encode (Str $to_encode) returns Str>

=item B<pack_params returns Str>

=item B<unpack_params (Str $data) returns Str>

=item B<as_yaml> - Returns the query parameters as a YAML string.

=back

=head1 Debugging

=head2 Dumping Out All the Name/Value pairs

The C<Dump> method produces a string consisting of all the query's
name/value pairs formatted nicely as a nested list.  This is useful
for debugging purposes:

    print $q.Dump;

Produces something that looks like:

    <ul>
        <li>name1
         <ul>
            <li>value1
            <li>value2
        </ul>
         <li>name2
            <ul>
                <li>value1
            </ul>
    </ul>

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

=end pod
# vim: ft=perl6
