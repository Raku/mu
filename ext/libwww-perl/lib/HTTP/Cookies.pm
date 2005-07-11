#!/usr/bin/pugs
use v6;

use HTTP::Date <str2time time2str>;
use HTTP::Headers::Util <split_header_words join_header_words>;

class HTTP::Cookies-0.0.1 {
    ## Class variables
    our $EPOCH_OFFSET;
    
    ## Attributes
    has %:cookies           is rw;
    
    has $.file              is rw;
    has $.autosave          is rw;
    has $.ignore_discard    is rw;
    has $.hide_cookie2      is rw;
    
    $EPOCH_OFFSET = 0; # difference from Unix epoch
    
    if ($*OS eq "MacOS") {
        require Time::Local;
        $EPOCH_OFFSET = Time::Local::timelocal(0,0,0,1,0,70);
    }
    
    ## Creation and destruction
    submethod BUILD (Str $.file, Bool ?$.autosave = 0, Bool ?$.ignore_discard = 0, Bool ?$.hide_cookie2 = 0) {
        ./load();
    }
    
    submethod DESTROY () {
        ./save() if $.autosave;
    }
    
    ## Instance methods
    method add_cookie_header ($request) {
        ...
    }
    
    method extract_cookies ($response) {
        ...
    }
    
    method set_cookie (Num $version, Str $key, Str $val, Str $path, Str $domain, Str $port, Bool $path_spec, Bool $secure, Num $maxage, Bool $discard, *%rest) {
        ...
    }
    
    method save (Str ?$file = $.file) {
        ...
    }
    
    method load (Str ?$file = $.file) {
        ...
    }
    
    method revert () {
        ...
    }
    
    multi method clear () {
        ...
    }
    
    multi method clear (Str $domain, Str ?$path, Str ?$key) {
        ...
    }
    
    method clear_temporary_cookies () {
        ...
    }
    
    method scan (Code $callback) {
        ...
    }
    
    method as_string (Bool ?$skip_discardables) {
        ...
    }
    
    ## Class methods
    # these may also be called on an instance, but they are not tied to a
    # particular instance
    method :host (HTTP::Request $request, URI $uri) {
        ...
    }
    
    method :url_path (URI $uri) {
        ...
    }
    
    method :normalize_path (Str $str) {
        ...
    }
}