#!/usr/bin/pugs
use v6;

use HTTP::Status ();
require HTTP::Message;

class HTTP::Response[?::URI_CLASS = URI] {
    is HTTP::Message[::URI_CLASS];
    
    has $.code      is rw;
    has $.message   is rw;
    has $.previous  is rw;
    has $.request   is rw;
    
    submethod BUILD (+$.code, +$.message) { }
    
    method parse (Str $str is copy) {
        my $status_line;
        
        if ($str ~~ s/^(.*)\n//) {
            $status_line = $0;
        } else {
            $status_line = $str;
            $str = "";
        }
        
        my $self = ../parse($str);
        
        given ($self) {
            my ($protocol, $code, $message);
            
            if ($status_line ~~ /^\d**{3}/) {
                # Looks like a response created by HTTP::Response.new
                ($code, $message) = $status_line.split(' ', 2);
            } else {
                ($protocol, $code, $message) = $status_line.split(' ', 3);
            }
            
            .protocol($protocol) if $protocol.defined;
            .code($code) if $code.defined;
            .message($message) if $message.defined;
        }
        
        $self;
    }
    
    method status_line () {
        my $code = .code // "000";
        my $mess = ./message // HTTP::Status::status_message($code) // "?";
        "$code $mess";
    }
    
    method base () {
        my $base = ./header('Content-Base')      //  # used to be HTTP/1.1
                   ./header('Content-Location')  //  # HTTP/1.1
                   ./header('Base');                 # HTTP/1.0
        
        require URI;
        
        if ($base.defined && $base ~~ /^ <URI::scheme> \:/) {
            # already absolute
            return $HTTP::URI_CLASS.new($base);
        }
        
        my $req = ./request;
        
        if ($req) {
            # if $base is undef here, the return value is effectively
            # just a copy of $self.request.uri.
            return $HTTP::URI_CLASS.new_abs($base, $req.uri);
        }
        
        # can't find an absolute base.
        return undef;
    }
    
    method as_string (Str ?$newline = "\n") {
        my $code = ./code;
        my $status_message = HTTP::Status::status_message($code) // "Unknown code";
        my $message = ./message // "";
        
        my $status_line = "$code";
        my $proto = ./protocol;
        $status_line = "$proto $status_line" if $proto.defined;
        $status_line ~= " ($status_message)" if $status_message ne $message;
        $status_line ~= " $message";
        
        return ($status_line, ../as_string($newline)).join($newline);
    }
    
    method is_info     () { HTTP::Status::is_info     (./code); }
    method is_success  () { HTTP::Status::is_success  (./code); }
    method is_redirect () { HTTP::Status::is_redirect (./code); }
    method is_error    () { HTTP::Status::is_error    (./code); }
    
    method error_as_HTML () {
        my $title = "An Error Occurred";
        my $body = ./status_line;
        
        return "<HTML>
<HEAD><TITLE>$title</TITLE></HEAD>
<BODY>
<H1>$title</H1>
$body
</BODY>
</HTML>";
    }
    
    method current_age () {
        my $response_time = ./client_date;
        my $date = ./date;
        
        my $age = 0;
        
        if ($response_time && $date) {
            $age = $response_time - $date; # apparent_age
            $age = 0 if $age < 0;
        }
        
        my $age_v = ./header('Age');
        
        if ($age_v && $age_v > $age) {
            $age = $age_v; # corrected_received_age
        }
        
        my $request = ./request;
        
        if ($request) {
            my $request_time = $request.date;
            
            if ($request_time) {
                # Add response_delay to age to get 'corrected_initial_age'
                $age += ($response_time - $request_time);
            }
        }
        
        if ($response_time) {
            $age += time - $response_time;
        }
        
        return $age;
    }
    
    method freshness_lifetime () {
        my @cc = ./header('Cache-Control');
        
        # First look for the Cache-Control: max-age=n header
        if (@cc) {
            for @cc -> $cc {
                for $cc.split(/\s*,\s*/) -> $cc_dir {
                    if ($cc_dir ~~ rx:i/max-age\s*=\s*(\d+)/) {
                        return $0;
                    }
                }
            }
        }
        
        # Next possibility is to look at the "Expires" header
        my $date = ./date // ./client_date // time;
        my $expires = ./expires;
        
        unless ($expires.defined) {
            # Must apply heuristic expiration
            my $last_modified = ./last_modified;
            
            if ($last_modified.defined) {
                my $h_exp = ($date - $last_modified) * 0.10; # 10% since last-mod
                
                if ($h_exp < 60) {
                    return 60; # minimum
                } elsif ($h_exp > (24 * 3600)) {
                    # Should give a warning if more than 24 hours according to
                    # RFC 2616 section 13.2.4, but I don't know how to do it
                    # from this function interface, so I just make this the
                    # maximum value.
                    return 24 * 3600;
                }
                
                return $h_exp;
            } else {
                return 3600; # 1 hour is fallback when all else fails
            }
        }
    
        return $expires - $date;
    }

    method is_fresh     () { ./freshness_lifetime > ./current_age }
    method fresh_until  () { ./freshness_lifetime - ./current_age + time }
}

1;
