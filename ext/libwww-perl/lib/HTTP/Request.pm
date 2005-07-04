#!/usr/bin/pugs
use v6;

use HTTP::Message;

class HTTP::Request-0.1 {
    is HTTP::Message;
    
    has $.method    is rw;
    has $:uri       is rw;
    
    submethod BUILD ($.method, $.uri, $header, $content) { }
    
    method parse ($str) {
        my $request_line;
        
        if ($str ~~ s/^(.*)\n//) {
            $request_line = $0;
        } else {
            $request_line = $str;
            $str = "";
        }
        
        my $self = .SUPER::parse($str);
        
        given ($self) {
            my ($method, $uri, $protocol) = $request_line.split(' ');
            $self.method($method) if $method.defined;
            $self.uri($uri) if $method.defined;
            $self.protocol($protocol) if $protocol.defined;
        }
        
        $self;
    }
    
    multi method uri (Str $new) is rw {
        my $old = $:uri;
    
        return new Proxy:
            FETCH => { $old; },
            STORE => { $:uri = $HTTP::URI_CLASS.new($^new); $old; };
    }
    
    multi method uri (URI $new) is rw {
        my $old = $:uri;
    
        return new Proxy:
            FETCH => { $old; },
            STORE => { $:uri = $^new; $old; };
    }
    
    multi method uri () {
        $:uri;
    }
    
    our &url := &uri;
    
    method as_string (Str ?$newline = "\n") {
        my $req_line = $.method // "-";
        my $uri = (.uri().defined) ?? .uri().as_string() :: "-";
        
        $req_line ~= $uri;
        
        my $proto = .protocol;
        
        $req_line ~= $protocol if $proto.defined;
        
        return ($newline, $req_line, .SUPER::as_string($newline)).join($newline);
    }
}