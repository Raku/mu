use v6;

use HTTP::Headers ();
use HTTP::Request ();
use HTTP::Response ();

class HTTP::Message-0.1;

our $CRLF = "\015\012";

has $:headers handles <
    header push_header init_header remove_header remove_content_headers header_field_names scan
   date expires if_modified_since if_unmodified_since last_modified
   content_type content_encoding content_length content_language
   title user_agent server from referer
   www_authenticate authorization proxy_authorization authorization_basic proxy_authorization_basic
>;

has $:content;
has $:content_ref;
has $:protocol;

has @:parts;

multi submethod BUILD (HTTP::Headers $header, Str ?$content = "") {
    $:headers = $header;
    $:content = $content;
}

multi submethod BUILD (%header, Str ?$content = "") {
    $:headers = HTTP::Headers.new(*%header);
    $:content = $content;
}

method parse (Str $string) returns HTTP::Headers {
    my %headers;
    
    my ($field, $value);
    
    loop {
        # XXX use Perl 6 pattern matching and named binding
        #if ($string ~~ s:/^ $field := (<-[ \t:]>+) <[ \t]>*\: <sp>? $value := (.*) \n?//) {
        if ($string ~~ s:P5/^([^ \t:]+)[ \t]*: ?(.*)\n?//) {
            $field = $0;
            $value = $1;
            
            #$value ~~ s/\r$//;
            $value ~~ s:P5/\r\z//;
            %headers{$field} = $value;
        } elsif (%headers.pairs && $string ~~ s/^ (<[ \t]> .*) \n?//) {
            %headers{$field} ~= "\n$0";
            #%headers{$field} ~~ s/\r$//;
            %headers{$field} ~~ s:P5/\r\z//;
        } else {
            $string ~~ s:P5/^\r?\n//;
            last;
        }
    }
    
    return ::?CLASS.new(%headers, $string);
}

method clear () returns Void {
    $:headers.clear();
    .content = '';
    @.:parts = ();
    return;
}

method protocol (Str ?$protocol) is rw {
    return new Proxy:
        FETCH => { $:protocol; },
        STORE => -> Str $val { $:protocol = $val; };
}

# XXX this might need to be rewritten
method content (Str ?$content, Bool ?$keep) is rw {
    return new Proxy:
        FETCH => {
                if (want.List) {
                    $:content unless $:content.defined;
                    
                    my $old = $:content;
                    $old = $$old if $old.ref eq "Scalar";
                    
                    return $old;
                }
              },
        STORE => -> Str $content, Bool ?$keep {
                if (want ~~ List) {
                    $:content unless $:content.defined;
                    
                    my $old = $self.:content;
                    $old = $$old if $old.ref eq "Scalar";
                }
                
                $:content = $content;
                
                #@:parts = () if $del;
                
                .:set_content($content, $keep);
                
                return $old if want.List;
              };
}

method :set_content (Str $content, Bool ?$keep) returns Void {
    $:content = $content;
    @:parts = () unless $keep;
}

# XXX does add_content() need to create references, etc. like the P5 version?
# I figure it shouldn't be needed since the parameters are bound, not copied.
method add_content (Str $content) {
    $:content ~= $content;
}

method content_ref (Ref ?$content) is rw {
    $:content unless $:content.defined;
    
    @:parts = ();
    
    my $old = \$:content;
    my $old_ref = $:content_ref;
    $old = $$old if $old_ref;
    
    return new Proxy:
        FETCH => { return $old; },
        STORE => -> Ref $content {
                $:content = $content;
                $:content_ref++;
                
                return $old;
            };
}

# XXX decoded_content needs to be ported.  It requires:
#   HTTP::Headers::Util (done)
#   Compress::Zlib
#   Compress::Bzip2
#   MIME::Base64
#   MIME::QuotedPrint
#   Encode
method decoded_content ($self: ) {
    ...
}

method as_string ($self: Str ?$newline = "\n") returns Str {
    my $content = .content;
    
    return ($:headers.as_string($newline), $newline, ($content.chars && $content !~ /\n$/) ?? "\n" :: "").join("");
}

method parts (*@new) is rw {
    my @old = @:parts;
    
    return new Proxy:
        FETCH => { return @old if want.List; return @old[0]; },
        STORE => -> *@new {
            my $content_type = .content_type // "";
            
            if ($content_type ~~ m,^message/,) {
                die "Only one part allowed for $content_type content!"
                    if @new > 1;
            } elsif ($content_type !~ m,^multipart/,) {
                .remove_content_headers;
                .content_type("multipart/mixed");
            }
            
            @:parts = @new;
            .:stale_content;
            
            return @old if want.List;
            return @old[0];
        }
}

method add_part (::?CLASS $part) returns Void {
    if ((.content_type // "") !~ m,^multipart/,) {
        my $message = ::?CLASS.new(.remove_content_headers, .content(""));
        .content_type("multipart/mixed");
        @:parts = $message;
    } elsif (!@:parts) {
        @:parts;
    }
    
    push @:parts, $part;
    .:stale_content;
    return;
}

method :stale_content () {
    undefine($:content);
    undefine($:content_ref);
}

method :parts () {
    my $content_type = .content_type;
    
    if ($content_type ~~ m,^multipart/,) {
        my @h = HTTP::Headers::Util::split_header_words(.header("Content-Type"));
        my %h = %{@h[0]};
        
        if ((my $boundary = $h<boundary>).defined) {
            my $str = .content;
            
            if ($str ~~ s:P5/(^|.*?\r?\n)--\Q$boundary\E\r?\n//) {
                @:parts = split(rx:P5/\r?\n--\Q$b\E\r?\n/, $str).map:{ HTTP::Message::parse($_); };
            }
        }
    } elsif ($content_type eq "message/http") {
        my $content = .content;
        my $class = ($content ~~ m,^(HTTP/.*)\n,) ?? HTTP::Response :: HTTP::Request;
        @:parts = $class.parse($content);
    } elsif ($content_type ~~ m,message/,) {
        @:parts = HTTP::Message.parse(.content);
    }
    
    @:parts //= ();
}

method :content () {
    my $content_type = .content_type // "";
    
    if ($content_type ~~ m:i,^\s*message/,) {
        .:set_content(@:parts[0].as_string($CRLF), 1);
        return;
    }
    
    my @v = HTTP::Headers::Util::split_header_words($content_type);
    die "Multiple Content-Type headers!" if @v > 1;
    
    @v = @v[0];
    
    my ($boundary, $boundary_index);
    
    my @tmp = @v;
    
    for @tmp -> $k, $v {
        if ($k.lc() eq "boundary") {
            $boundary = $v;
            $boundary_index = @v - @tmp - 1;
            last;
        }
    }
    
    my @parts = @:parts.map({ .as_string($CRLF) });
    
    my $bno = 0;
    $boundary //= .:boundary;
    
    # XXX need to do the CHECK_BOUNDARY bit
    
    if ($boundary_index) {
        @v[$boundary_index] = $boundary;
    } else {
        @v.push(boundary => "boundary");
    }
    
    $content_type = HTTP::Headers::Util::join_header_words(@v);
    .header("Content-Type", $content_type);
    
    .:set_content("--$boundary$CRLF" ~ @parts.join("$CRLF--boundary$CRLF") ~ "$CRLF--$boundary$CRLF", 1);
}

method :boundary (Num ?$size) returns Str {
    if (!$size) { return "xYzZY"; }
    
    my $b;
    # XXX use MIME::Base64::encode
    #$b = MIME::Base64::encode(1..($size * 3).map:{ chr(rand(256)) }.join(""), "");
    $b ~~ s:g/\W+/X/; # ensure alnum only
    return $b;
}

multi method *coerce:<as> (Str ::to) {
    .as_string("\n");
}
